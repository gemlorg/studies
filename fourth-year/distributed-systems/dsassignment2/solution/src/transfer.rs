pub(crate) mod transfer_impl {
    static READ_CODE: u8 = 0x1;
    static WRITE_CODE: u8 = 0x2;
    static READ_PROC_CODE: u8 = 0x3;
    static VALUE_CODE: u8 = 0x4;
    static WRITE_PROC_CODE: u8 = 0x5;
    static ACK_CODE: u8 = 0x6;

    use hmac::Mac;
    use log::debug;
    use std::io::Error;
    use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

    use crate::{
        ClientRegisterCommand, OperationReturn, OperationSuccess, RegisterCommand, SectorVec,
        StatusCode, SystemRegisterCommand, MAGIC_NUMBER,
    };

    trait BinSerializable {
        fn serialize(&self) -> Vec<u8>;
    }

    impl BinSerializable for SystemRegisterCommand {
        fn serialize(&self) -> Vec<u8> {
            let (op_type, op_data) = match self.clone().content {
                crate::SystemRegisterCommandContent::ReadProc => (READ_PROC_CODE, vec![]),
                crate::SystemRegisterCommandContent::Value {
                    timestamp,
                    write_rank,
                    sector_data,
                } => (
                    VALUE_CODE,
                    vec![
                        timestamp.to_be_bytes().to_vec(),
                        vec![0u8; 7],
                        write_rank.to_be_bytes().to_vec(),
                        sector_data.0.to_vec(),
                    ],
                ),
                crate::SystemRegisterCommandContent::WriteProc {
                    timestamp,
                    write_rank,
                    data_to_write,
                } => (
                    WRITE_PROC_CODE,
                    vec![
                        timestamp.to_be_bytes().to_vec(),
                        vec![0u8; 7],
                        write_rank.to_be_bytes().to_vec(),
                        data_to_write.0.to_vec(),
                    ],
                ),
                crate::SystemRegisterCommandContent::Ack => (ACK_CODE, vec![]),
            };
            let mut res: Vec<u8> = Vec::new();
            // 0 - 31
            res.extend(&MAGIC_NUMBER);
            // 32 - 47
            res.extend(vec![0u8; 2]);
            // 48 - 55
            res.extend(self.header.process_identifier.to_be_bytes());
            // 56 - 64
            res.extend((op_type).to_be_bytes());
            // UUID
            res.extend(self.header.msg_ident.as_bytes());
            // sector index
            res.extend(self.header.sector_idx.to_be_bytes());

            // rest is data
            res.extend(op_data.iter().flat_map(|x| x.iter()));
            res
        }
    }
    impl BinSerializable for ClientRegisterCommand {
        fn serialize(&self) -> Vec<u8> {
            let mut res: Vec<u8> = Vec::new();
            let (op_type, op_data) = match self.clone().content {
                crate::ClientRegisterCommandContent::Read => (READ_CODE, vec![]),
                crate::ClientRegisterCommandContent::Write { data } => {
                    (WRITE_CODE, vec![data.0.to_vec()])
                }
            };
            // 0 - 31
            res.extend(&MAGIC_NUMBER);
            // 32 - 47
            res.extend(vec![0u8; 2]);
            //48 - 55 , status code
            // always ok
            res.extend(vec![0u8; 1]);
            // 56 - 63
            res.extend((op_type).to_be_bytes());
            // request identifier
            res.extend(self.header.request_identifier.to_be_bytes());
            // sector index
            res.extend(self.header.sector_idx.to_be_bytes());
            // rest is data
            res.extend(op_data.iter().flat_map(|x| x.iter()));
            res
        }
    }

    pub(crate) async fn serialize_register_command(
        cmd: &RegisterCommand,
        writer: &mut (dyn AsyncWrite + Send + Unpin),
        hmac_key: &[u8],
    ) -> Result<(), Error> {
        let mut cmd_serialized = match cmd {
            RegisterCommand::Client(client_register_command) => client_register_command.serialize(),
            RegisterCommand::System(system_register_command) => system_register_command.serialize(),
        };
        let mut hmac = hmac::Hmac::<sha2::Sha256>::new_from_slice(hmac_key).unwrap();
        hmac.update(&cmd_serialized);
        cmd_serialized.extend(hmac.finalize().into_bytes().iter());
        writer.write_all(&cmd_serialized).await
    }

    pub(crate) async fn deserialize_register_command(
        data: &mut (dyn AsyncRead + Send + Unpin),
        hmac_system_key: &[u8; 64],
        hmac_client_key: &[u8; 32],
    ) -> Result<(RegisterCommand, bool), Error> {
        let cmd: RegisterCommand;
        let mut hmac: hmac::Hmac<sha2::Sha256>;
        let mut buf = [0u8; 8];
        data.read_exact(&mut buf[0..4]).await?;
        while buf[0..4] != MAGIC_NUMBER {
            debug!("Magic number mismatch, rotating buffer");
            buf.rotate_left(1);
            data.read_exact(&mut buf[3..4]).await?;
        }
        data.read_exact(&mut buf[4..8]).await?;
        let msg_type = buf[7];
        match msg_type {
            1_u8..=2_u8 => {
                hmac = hmac::Hmac::<sha2::Sha256>::new_from_slice(hmac_client_key).unwrap();
                hmac.update(&buf);
                //request number
                data.read_exact(&mut buf).await?;
                hmac.update(&buf);
                let request_number = u64::from_be_bytes(buf);
                data.read_exact(&mut buf).await?;
                hmac.update(&buf);
                let sector_idx = u64::from_be_bytes(buf);
                let header = crate::ClientCommandHeader {
                    request_identifier: request_number,
                    sector_idx,
                };

                cmd = match msg_type {
                    1u8 => RegisterCommand::Client(ClientRegisterCommand {
                        header,
                        content: crate::ClientRegisterCommandContent::Read,
                    }),
                    2u8 => {
                        let mut data_buf = [0u8; 4096];
                        data.read_exact(&mut data_buf).await?;
                        hmac.update(&data_buf);
                        let vec_data = SectorVec(data_buf.to_vec());
                        RegisterCommand::Client(ClientRegisterCommand {
                            header,
                            content: crate::ClientRegisterCommandContent::Write { data: vec_data },
                        })
                    }
                    _ => unreachable!(),
                };
            }
            3u8..=6u8 => {
                hmac = hmac::Hmac::<sha2::Sha256>::new_from_slice(hmac_system_key).unwrap();
                hmac.update(&buf);
                let process_rank = buf[6];
                let mut uuid_buf = [0u8; 16];
                data.read_exact(&mut uuid_buf).await?;
                hmac.update(&uuid_buf);
                let uuid = uuid::Uuid::from_slice(&uuid_buf).unwrap();
                data.read_exact(&mut buf).await?;
                hmac.update(&buf);
                let sector_idx = u64::from_be_bytes(buf);
                let header = crate::SystemCommandHeader {
                    process_identifier: process_rank,
                    msg_ident: uuid,
                    sector_idx,
                };
                cmd = RegisterCommand::System(SystemRegisterCommand {
                    header,
                    content: match msg_type {
                        3u8 => crate::SystemRegisterCommandContent::ReadProc,
                        4u8..=5u8 => {
                            data.read_exact(&mut buf).await?;
                            hmac.update(&buf);
                            let timestamp = u64::from_be_bytes(buf);
                            data.read_exact(&mut buf).await?;
                            hmac.update(&buf);
                            let write_rank = buf[7];
                            let mut sector_data = [0u8; 4096];
                            data.read_exact(&mut sector_data).await?;
                            hmac.update(&sector_data);
                            match msg_type {
                                4u8 => crate::SystemRegisterCommandContent::Value {
                                    timestamp,
                                    write_rank,
                                    sector_data: SectorVec(sector_data.to_vec()),
                                },
                                5u8 => crate::SystemRegisterCommandContent::WriteProc {
                                    timestamp,
                                    write_rank,
                                    data_to_write: SectorVec(sector_data.to_vec()),
                                },
                                _ => unreachable!(),
                            }
                        }
                        6u8 => crate::SystemRegisterCommandContent::Ack,
                        _ => unreachable!("Invalid message type: {}", msg_type),
                    },
                })
            }

            _ => {
                return Err(Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Invalid message type: {}", msg_type),
                ))
            }
        }
        let mut hmac_buf: [u8; 32] = [0u8; 32];
        data.read_exact(&mut hmac_buf).await?;
        Ok((cmd, hmac.verify((&hmac_buf).into()).is_ok()))
    }

    pub(crate) async fn serialize_success(
        op_succ: OperationSuccess,
        writer: &mut (dyn AsyncWrite + Send + Unpin),
        hmac_key: &[u8],
    ) -> Result<(), Error> {
        let mut res: Vec<u8> = Vec::new();
        // 0 - 31
        res.extend(&MAGIC_NUMBER);
        // 32 - 47
        res.extend(vec![0u8; 2]);
        //48 - 55 , status code
        // always ok
        res.extend(vec![0u8; 1]);
        // 56 - 63
        let (typ, content): (u8, Vec<u8>) = match op_succ.op_return {
            OperationReturn::Read(rreturn) => (0x41, rreturn.read_data.0.to_vec()),
            OperationReturn::Write => (0x42, vec![]),
        };
        res.extend((typ).to_be_bytes());
        res.extend((op_succ.request_identifier).to_be_bytes());
        res.extend(content);
        let mut hmac = hmac::Hmac::<sha2::Sha256>::new_from_slice(hmac_key).unwrap();
        hmac.update(&res);
        res.extend(hmac.finalize().into_bytes().iter());
        writer.write_all(&res).await
    }

    pub(crate) async fn serialize_fail(
        op_fail: StatusCode,
        cmd: ClientRegisterCommand,
        writer: &mut (dyn AsyncWrite + Send + Unpin),
        hmac_key: &[u8],
    ) -> Result<(), Error> {
        let mut res: Vec<u8> = Vec::new();
        // 0 - 31
        res.extend(&MAGIC_NUMBER);
        // 32 - 47
        res.extend(vec![0u8; 2]);
        //48 - 55 , status code
        // always ok
        let fail_code: u8 = match op_fail {
            StatusCode::AuthFailure => 0x1,
            StatusCode::InvalidSectorIndex => 0x2,
            StatusCode::Ok => unreachable!(),
        };
        res.extend(fail_code.to_be_bytes());
        // 56 - 63
        let typ: u8 = match cmd.content {
            crate::ClientRegisterCommandContent::Read => 0x41,
            crate::ClientRegisterCommandContent::Write { data: _ } => 0x42,
        };
        res.extend((typ).to_be_bytes());
        res.extend((cmd.header.request_identifier).to_be_bytes());
        let mut hmac = hmac::Hmac::<sha2::Sha256>::new_from_slice(hmac_key).unwrap();
        hmac.update(&res);
        res.extend(hmac.finalize().into_bytes().iter());
        writer.write_all(&res).await
    }
}
