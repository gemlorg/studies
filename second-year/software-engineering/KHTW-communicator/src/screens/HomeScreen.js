import { StyleSheet, Text, View, TouchableOpacity, SafeAreaView, FlatList, Pressable, Image, TextInput, Button, ScrollView} from 'react-native'
import { auth, findUserByMail } from '../firebase'
import { signOut } from 'firebase/auth'
import { useNavigation } from '@react-navigation/core'
import React, {useEffect, useState } from "react";

import {
    getDatabase,
    get,
    ref,
    set,
    onValue,
    push,
    update,
    orderByChild,
    getReference
  } from 'firebase/database';


const HomeScreen = ({route}) => {
    const [users, setUsers] = useState([]);
    const [userToAdd, setUserToAdd] = useState(null);
    const [nameToAdd, setNameToAdd] = useState(null);

    const [myData, setMyData] = useState(null);
    const navigation = useNavigation()
    const { username, mail, avatar } = route.params;

    
    useEffect(() => {
       
        const database = getDatabase();

        const myUserRef = ref(database, `users/${username}`);
        onValue(myUserRef, snapshot => {
            const data = snapshot.val();
            setUsers(data.chats);
        });
    }, [])

   

  const handleSignOut = () => {
    signOut(auth)
      .then(() => {
        navigation.replace("Login");
      })
      .catch((error) => alert(error.message));
  };

    const renderUser=({item}) => {
        return <Pressable onPress={()=> { navigation.replace("Chat", { firstUser:username, firstAvatar: avatar, firstMail: mail, group: item.users, chatroomId: item.chatroomId, latex: "" });
        } } style = {styles.row}>
            <Image style={styles.avatar} source={{uri: item.avatar}}/>
            <Text> {item.name} </Text>
        </Pressable>
    };

    const findUser = async name => {
        const database = getDatabase();
    
        const mySnapshot = await get(ref(database, `users/${name}`));
    
        return mySnapshot.val();
      };

      const getUsersByEmail = async (mails) => {
        const usersa = [];
        for (let index = 0; index < mails.length; index++) {
            const user = await findUserByMail(mails[index]);
            if (user.length === 0) {
                    console.log("no user with this email");
            } else {
                usersa.push(user[0]);
            }

        }
        return usersa;

    };


    const onAddFriend = async (names, groupName) => {
        try {

            const database = getDatabase();
            const me = await findUser(username);            
            setMyData(me);
            //const user = await findUser(name);
            namesLoverCase = names.toLowerCase();
            namesList = namesLoverCase.split(" ");
            namesList.push(mail);
            const userss = await getUsersByEmail(namesList);
            const userInfo = userss.map(user => {
                const { username, mail, avatar } = user; // Specify the attributes you want to save
                return {  username, mail, avatar }; // Create a new object with the desired attributes
            });

            if (userss.length > 0) {
                const newChatroomRef = push(ref(database, 'chatrooms'), {
                    name:groupName,
                    users:userInfo,
                    messages: [],
                  });
          
                  const newChatroomId = newChatroomRef.key;
                  
                
                  
                  //join myself to this user friend list

                  for (let index = 0; index < userss.length; index++) {
                    const thisUser = userss[index];
                    const userFriends = thisUser.chats || [];
                    update(ref(database, `users/${thisUser.username}`), {
                                        chats: [
                                        ...userFriends,
                                        {
                                            name:groupName,
                                            avatar: 'https://picsum.photos/200/300?random='+Date.now(),
                                            chatroomId: newChatroomId,
                                            users: userInfo,
                                        },
                                        ],
                                    });

                  }
                  
          
                  //const myFriends = me.chats || [];
                  //add this user to my friend list
                  //update(ref(database, `users/${me.username}`), {
                   // friends: [
                    //  ...myFriends,
                    //  {
                    //    name:"groupchat",
                    //    avatar: me.avatar,
                    //    chatroomId: newChatroomId,
                    //    users: userss,
                    //  },
                 //   ],
                //  });
                
            }


        } catch (error) {
            console.error(error);
        }
    };

    return (
        <>


        <View style={styles.addUser}>
            <TextInput
            style={styles.input}
            onChangeText={setNameToAdd}
            value={nameToAdd}
            />
            <TextInput
            style={styles.input}
            onChangeText={setUserToAdd}
            value={userToAdd}
            />
            <Button title={'Add Chat'} onPress={() => onAddFriend(userToAdd, nameToAdd)} />
        </View>
        <View>
            <FlatList
                data={users}
                renderItem={renderUser}
                keyExtractor={item=>item.chatroomId.toString()} />
        </View>

      <View style={styles.container}>
        <TouchableOpacity onPress={handleSignOut} style={styles.button}>
          <Text style={styles.buttonText}>Sign out</Text>
        </TouchableOpacity>
      </View>
    </>
  );
};

export default HomeScreen;

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center",
  },
  list: {
    flex: 1,
    justifyContent: "left",
    alignItems: "left",
  },

  button: {
    backgroundColor: "blue",
    width: "60%",
    padding: 15,
    borderRadius: 5,
    alignItems: "center",
    marginTop: 40,
    position: "absolute",
    bottom: 20,
  },

  buttonText: {
    color: "white",
    fontWeight: "700",
    fontSize: 16,
  },

  buttonOutline: {
    backgroundColor: "white",
    marginTop: 5,
    borderColor: "blue",
    borderWidth: 2,
  },

    buttonOutlineText: {
        color: 'blue',
        fontWeight: '700',
        fontSize: 16,

    },
    row: {
        flexDirection:'row',
        padding:10,
        alignItems:'center',
        borderBottomColor:'#cacaca',
        borderBottomWidth:1,
    },
    avatar: {
        width:50,
        height:50,
        marginRight:10,
    },
    addUser: {
        flexDirection: 'row',
        padding: 10,
    },
    input: {
        borderRadius: 10,
        backgroundColor: '#cacaca',
        flex: 1,
        marginRight: 10,
        padding: 10,
    },

})
