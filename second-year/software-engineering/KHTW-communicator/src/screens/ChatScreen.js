import React, { useCallback, useEffect, useState } from 'react'
import {
  getDatabase,
  get,
  ref,
  onValue,
  off,
  update,
  orderByChild,
  where,
  query,
} from 'firebase/database'
import {
  Image,
  Pressable,
  StyleSheet,
  Button,
  Modal,
  Text,
  View,
  TextInput,
  TouchableOpacity,
  TouchableWithoutFeedback,
  SafeAreaView,
} from 'react-native'
import { GiftedChat, InputToolbar, Actions } from 'react-native-gifted-chat'
import { useNavigation } from '@react-navigation/core'
import MathJax from 'react-native-mathjax'

// options for MathJax
const mmlOptions = {
  messageStyle: 'none',
  extensions: ['tex2jax.js'],
  jax: ['input/TeX', 'output/HTML-CSS'],
  tex2jax: {
    inlineMath: [
      ['$', '$'],
      ['\\(', '\\)'],
    ],
    displayMath: [
      ['$$', '$$'],
      ['\\[', '\\]'],
    ],
    processEscapes: true,
  },
  TeX: {
    extensions: [
      'AMSmath.js',
      'AMSsymbols.js',
      'noErrors.js',
      'noUndefined.js',
    ],
  },
}

export default function ChatScreen({ route }) {
  const [messages, setMessages] = useState([])
  const navigation = useNavigation()
  const {
    firstUser,
    firstMail,
    group,
    chatroomId,
    latex,
  } = route.params
  const firstAvatar = 'https://i.pravatar.cc/150?u=' + Date.now();
  const [text, setText] = useState('')

  const [myData, setMyData] = useState(null)
  const [selectedUsers, setSelectedUsers] = useState(null)

  useEffect(() => {
    //load old messages
    const loadData = async () => {
      setText(latex)
      console.log('mess', latex, text)

      const myChatroom = await fetchMessages()
      const user1 = await findUser(firstUser)

      const gr = myChatroom.users;
      setMyData(user1)
    
      setSelectedUsers(gr)

      setMessages(renderMessages(myChatroom.messages))
    }

    loadData()

    // set chatroom change listener
    const database = getDatabase()
    const chatroomRef = ref(database, `chatrooms/${chatroomId}`)
    onValue(chatroomRef, (snapshot) => {
      const data = snapshot.val()
      setMessages(renderMessages(data.messages))
    })

    return () => {
      //remove chatroom listener
      off(chatroomRef)
    }
  }, [])

  const findUser = async (name) => {
    const database = getDatabase()

    const mySnapshot = await get(ref(database, `users/${name}`))

    return mySnapshot.val()
  }

  const fetchMessages = useCallback(async () => {
    const database = getDatabase()

    const snapshot = await get(ref(database, `chatrooms/${chatroomId}`))

    const user1 = await findUser(firstUser)

    setMyData(user1)

    return snapshot.val()
  }, [chatroomId])

  const getSender =  (name) => {
        const sender = group.find(item => item.username === name);
        console.log("here", sender.avatar);
        return sender;
  }

  const customtInputToolbar = (props) => (
    <InputToolbar
      {...props}
      containerStyle={{
        backgroundColor: 'white',
      }}
    />
  )

  const renderMessages = useCallback((msgs) => {

    return msgs
      ? msgs.reverse().map( (msg, index) => ({
          ...msg,
          _id: index,
          user: {
            _id: msg.sender === firstUser ? firstUser : msg.sender,
            avatar: msg.sender === firstUser ? "" : "",
            name: msg.sender === firstMail ? firstUser : msg.mail,
          },
        }))
      : []
  }, [])

  

  const onSend = useCallback(
    async (msg = []) => {
      //send the msg[0] to the other user
      const database = getDatabase()

      //fetch fresh messages from server
      const currentChatroom = await fetchMessages()

      const lastMessages = currentChatroom.messages || []

      update(ref(database, `chatrooms/${chatroomId}`), {
        messages: [
          ...lastMessages,
          {
            text: msg[0].text,
            sender: firstUser,
            createdAt: new Date(),
            avatar: firstAvatar,
            mail: firstMail
          },
        ],
      })

      setMessages((prevMessages) => GiftedChat.append(prevMessages, msg))
    },
    [fetchMessages, firstUser, chatroomId]
  )

  function CustomMessage({ message }) {
    const { text, user } = message

    return (
      <View style={styles.message}>
        {/* <Text> hi</Text> */}
        <MathJax style={styles.latex} mathJaxOptions={mmlOptions} html={text} />
      </View>
    )
  }

  const formulasWindow = () => {
    const formulas = [
      'Pythagorean theorem: a² + b² = c²',
      'Quadratic formula: x = (-b ± sqrt(b² - 4ac)) / 2a',
      'Sum of angles in a triangle: 180°',
      'Area of a circle: A = πr²',
      // add more formulas as needed
    ]
    return (
      <View style={styles.container}>
        {formulas.map((formula, index) => (
          <View key={index} style={styles.formulaContainer}>
            <Text style={styles.formulaText}>{formula}</Text>
          </View>
        ))}
      </View>
    )
  }

  handleActionPress = () => {
    return (
      <View
        style={{
          flexDirection: 'row',
          height: 100,
          padding: 20,
        }}
      >
        <View style={{ backgroundColor: 'blue', flex: 0.3 }} />
        <View style={{ backgroundColor: 'red', flex: 0.5 }} />
        <Text>Hello World!</Text>
      </View>
    )
  }

  const renderActions = (props) => {
    return (
      <Pressable
        onPress={() => {
          navigation.replace('LaTeX', {
            firstUser: firstUser,
            firstAvatar: firstAvatar,
            firstMail: firstMail,
            group: group,
            chatroomId: chatroomId,
            latex: text,
          })
        }}
        style={styles.latexButton}
      >
        <Text style={styles.latexbtn}>LaTeX</Text>
      </Pressable>
    )
  }

  return (
    <>
      <Pressable
        onPress={() => {
          navigation.replace('Home', {
            username: firstUser,
            mail: firstMail,
            avatar: firstAvatar,
          })
        }}
        style={styles.actionBar}
      >
        <Image source={require('../assets/back.png')} />
      </Pressable>

      <GiftedChat
        renderInputToolbar={(props) => customtInputToolbar(props)}
        messages={messages}
        onSend={(messages) => onSend(messages)}
        renderUsernameOnMessage={true}
        initialText={text}
        onInputTextChanged={(text) => {
          setText(text)
        }}
        user={{ _id: firstUser,
                name: firstMail,
                avatar: firstAvatar,
        }}
        renderActions={() => renderActions()}
        renderMessageText={({ currentMessage }) => (
          <CustomMessage message={currentMessage} />
        )}
      />
    </>
  )
}

const styles = StyleSheet.create({
  formulas: {
    height: 500,
    width: 500,
    position: 'absolute',
    backgroundColor: 'powderblue',
  },

  container: {
    flex: 1,
    padding: 20,
  },
  formulaContainer: {
    backgroundColor: '#ffffff',
    borderRadius: 10,
    padding: 10,
    marginVertical: 5,
    shadowColor: '#000000',
    shadowOffset: {
      width: 0,
      height: 1,
    },
    shadowOpacity: 0.22,
    shadowRadius: 2.22,
    elevation: 3,
  },
  formulaText: {
    fontSize: 18,
    fontWeight: 'bold',
    color: '#000000',
  },

  actionBar: {
    backgroundColor: '#cacaca',
    height: 41,
    width: '100%',
    flexDirection: 'row',
    alignItems: 'center',
  },
  // container: {
  //   position:'relative',
  //   height: 30,
  //   flex: 1,
  //   margin: 30,
  // },
  // text: {
  //   fontSize: 20,
  //   fontWeight: "500",
  //   marginTop: 20,
  // },
  message: {
    flex: 0,
    // margin: 30,
    // backgroundColor: colors.chatPurple,
    // borderRadius: 30,
    // borderBottomRightRadius: 30,
    // marginBottom: 10,
    padding: 5,
    right: 15,
    justifyContent: 'flex-end',
    alignSelf: 'stretch',
    marginLeft: 10,
  },
  latex: {
    minWidth: 150,

    fontSize: 10,
    backgroundColor: 0,
  },
  latexbtn: {
    borderRadius: 10,
    height: 50,
    backgroundColor: '#cacaca',
    fontSize: 14,
    paddingBottom: 5,
    paddingTop: 5,
    paddingLeft: 10,
    paddingRight: 10,
  },
  latexButton: {
    height: '100%',
  },
})
