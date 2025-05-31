import { Button, Container, Fade } from '@mui/material';
import React from 'react';
import { useNavigate } from 'react-router-dom';

const EmptyCart = () => {
    const navigate = useNavigate();

    return (
        <Fade in={true}>
            <Container sx={{ display: 'flex', justifyContent: 'center', alignItems: 'center', height: '100%' }}>
                <div className='text-center md:space-y-4 space-y-3.5 text-gray-500'>
                    <h6 className='text-sm'>Nie ma produktów na liście</h6>
                    <Button
                        onClick={() => navigate('/products')}
                        size='large'
                        color='success'
                        sx={{ textTransform: 'capitalize' }}
                        variant='outlined'>
                        Kontynuuj zakupy
                    </Button>
                </div>
            </Container>
        </Fade>
    );
};

export default EmptyCart;
