import React, { useEffect, useState } from 'react'
import { StyleSheet, Text, View } from 'react-native'
import { KeyboardAvoidingView, TextInput, TouchableOpacity } from 'react-native'
import { auth } from '../firebase'
import { createUserWithEmailAndPassword, onAuthStateChanged, signInWithEmailAndPassword } from 'firebase/auth'
import { useNavigation } from '@react-navigation/core'
import {
    getDatabase,
    get,
    ref,
    set,
    onValue,
    push,
    update,
  } from 'firebase/database';

const LoginScreen = () => {
    const [email, setEmail] = useState("")
    const [password, setPassword] = useState("")
    const [myData, setMyData] = useState(null);

    const navigation = useNavigation()

    useEffect(() => {
        const unsubscribe = onAuthStateChanged(auth, (user) => {
            if (user) {
                               
            }
        })

        return unsubscribe
    }, [])



    const handleSignup = () => {
        createUserWithEmailAndPassword(auth, email, password)
            .then(userCredentials => {
                const user = userCredentials.user;
                console.log("Registered with: ", user.email);
                const user_id = user.uid;
                const avatar = 'https://i.pravatar.cc/150?u=' + Date.now()

                const newUserObj = {
                    username: user_id,
                    mail: user.email,
                    avatar: avatar,
                  };
                  const database = getDatabase();
                  set(ref(database, `users/${user_id}`), newUserObj);
                  setMyData(user_id);
                  navigation.navigate("Home", {
                    username: user_id,
                    mail: user.email,
                    avatar: avatar
                });
            })
            .catch(error => alert(error.message))
    }

    const handleLogin = () => {
        signInWithEmailAndPassword(auth, email, password)
            .then(userCredentials => {
                const user = userCredentials.user;
                console.log("Logged in with: ", user.email);
                const user_id = user.uid;
                setMyData(user_id);
                navigation.navigate("Home", {
                    username: user_id,
                    mail: user.email,
                    avatar: user.avatar
                });
            })
            .catch(error => alert(error.message))

    }

    return (

        <>

        <KeyboardAvoidingView
            style={styles.container}
            behaviour="padding"
        >

            <Text style={styles.title}>  KHTW - communicator </Text>
            <Text> Login or register with email below </Text>
            <View style={styles.inputContainer}>
                <TextInput
                    placeholder="Email"
                    value={email}
                    onChangeText={text => setEmail(text)}
                    style={styles.input}
                />
                <TextInput
                    placeholder="Password"
                    value={password}
                    onChangeText={text => setPassword(text)}
                    style={styles.input}
                    secureTextEntry
                />
            </View>
            <View style={styles.buttonContainer}>
                <TouchableOpacity
                    onPress={handleLogin}
                    style={styles.button}
                >
                    <Text style={styles.buttonText}>Login</Text>
                </TouchableOpacity>

                <TouchableOpacity
                    // onPress={() => { }}
                    onPress={handleSignup}
                    style={[styles.button, styles.buttonOutline]}
                >
                    <Text style={styles.buttonOutlineText}>Register</Text>
                </TouchableOpacity>
            </View>
        </KeyboardAvoidingView>
        </>
    )
}

export default LoginScreen

// =====================Styles ================================
const styles = StyleSheet.create({
    container: {
        flex: 1,
        justifyContent: 'center',
        alignItems: 'center'
    },

    inputContainer: {
        width: '80%'
    },

    input: {
        backgroundColor: 'white',
        paddingHorizontal: 10,
        paddingVertical: 10,
        borderRadius: 5,
        marginTop: 5
    },

    buttonContainer: {
        width: '60%',
        justifyContent: 'center',
        alignItems: 'center',
        marginTop: 40,
    },

    button: {
        backgroundColor: 'blue',
        width: '100%',
        padding: 15,
        borderRadius: 5,
        alignItems: 'center',
    },

    buttonText: {
        color: 'white',
        fontWeight: '700',
        fontSize: 16,

    },

    buttonOutline: {
        backgroundColor: 'white',
        marginTop: 5,
        borderColor: 'blue',
        borderWidth: 2,

    },

    buttonOutlineText: {
        color: 'blue',
        fontWeight: '700',
        fontSize: 16,

    },
    title: {
        color: 'blue',
        fontWeight: '700',
        fontSize: 20,
        textAlign: 'center',
        marginBottom: 5
    }


})
