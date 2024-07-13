import React, { useState } from 'react';
import {
  Pressable,
  StyleSheet,
  Text,
  View,
  ScrollView,
  SafeAreaView,
  TextInput,
  FlatList,
  KeyboardAvoidingView,
} from 'react-native';
import { useNavigation } from '@react-navigation/core';
import MathJax from 'react-native-mathjax';
import { formulas_pool } from './Formulas.js';

// const formulas_pool = [

// ]
//
//

const LaTeXScreen = ({ route }) => {
  const navigation = useNavigation();

  const { firstUser, firstAvatar, firstMail, group, chatroomId, latex } =
    route.params;
  const [message, setMessage] = useState(latex);
  const [find, setFind] = useState('');
  const [formulas, setFormulas] = useState(formulas_pool);
  const [position, setPosition] = useState(0);
  //const [inExpression, setInExpression] = useState(false);

  const parse = () => {
    if (position == 0) {
      // setInExpression(false);
      return false;
    }
    var inExp = false;
    var inlineNotDispExp = true; // true: inline, false: display
    for (let index = 0; index < position; index++) {
      if (message[index] == '\\') {
        index++;
        continue;
      }
      if (message[index] == '$') {
        if (index > 0 && message[index - 1] == '$') {
          // double $
          if (inExp) inlineNotDispExp = false; // beginning display
          else if (!inExp && inlineNotDispExp) inExp = true;
          else if (!inExp && !inlineNotDispExp);
        } else {
          // single $
          if (inExp) inExp = false;
          else if (!inExp) {
            inExp = true;
            inlineNotDispExp = true;
          }
        }
      }
      console.log(message[index], ' ', inExp);
    }
    console.log(inExp);
    //setInExpression(inExp);
    return inExp;
  };

  const insertText = (text) => {
    console.log(message, ' TEXT: ', text, ' POS: ', position);
    const prefix = message.slice(0, position);
    const suffix = message.slice(position, message.length);
    setMessage(prefix + text + suffix);
    setPosition(position + text.length);
  };

  const appendFormula = (item) => {
    const inExpression = parse();
    console.log('Cursor is in expression: ', inExpression);
    if (inExpression) insertText(item.formula);
    else insertText('$' + item.formula + '$');
    item.weight++;
  };

  const filterFormulas = () => {
    // if(find == "") {
    //   setFormulas(formulas_pool)
    //   return;
    // }
    // let s = '/*' + find + '*$/';
    // let regex = new RegExp(s )
    var l = [];
    var k = 30;
    for (var i = 0; i < formulas_pool.length && k >= 0; i++) {
      if (
        formulas_pool[i].title.toUpperCase().includes(find.toUpperCase()) ||
        formulas_pool[i].formula.toUpperCase().includes(find.toUpperCase())
      ) {
        k--;
        l.push(formulas_pool[i]);
        formulas_pool[i].weight++;
      }
    }
    setFind('');
    setFormulas(l);
  };

  const Item = ({ item }) => (
    <Pressable
      onPress={() => {
        appendFormula(item);
      }}>
      <View style={styles.list_item}>
        <MathJax
          style={styles.list_item_title_1}
          mathJaxOptions={mmlOptions}
          html={item.title + ' ' + '$' + item.formula + '$'}
        />
      </View>
    </Pressable>
  );

  return (
    <SafeAreaView>
      <KeyboardAvoidingView
        behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
        keyboardVerticalOffset={60}
        style={styles.wrapper}>
        <View style={styles.compiled_wrapper}>
          <ScrollView style={styles.compiled_text_wrapper}>
            <View style={styles.compiled}>
              <MathJax
                style={styles.latex}
                mathJaxOptions={mmlOptions}
                html={message}
              />
            </View>
          </ScrollView>
        </View>

        <View style={styles.button_wrapper}>
          <View style={styles.formulas_search}>
            <View style={styles.find_wrapper}>
              <TextInput
                style={styles.list_find_text}
                spellCheck={false}
                onChangeText={(text) => {
                  setFind(text);
                  console.log(find);
                }}
                value={find}
                placeholder="Find a formula"
              />
            </View>

            <View style={styles.find_button_wrapper}>
              <Pressable
                style={styles.find_button}
                onPress={() => {
                  filterFormulas();
                }}>
                <Text style={styles.find_text}>Find</Text>
              </Pressable>
            </View>
          </View>

          <View style={styles.list_bg}>
            <FlatList
              data={formulas.sort((a, b) =>
                b.weight.toString().localeCompare(a.weight)
              )}
              renderItem={({ item }) => (
                //<Item item={item} />
                <Pressable
                  onPress={() => {
                    appendFormula(item);
                  }}>
                  <View style={styles.list_item}>
                    <MathJax
                      style={styles.list_item_title_1}
                      mathJaxOptions={mmlOptions}
                      html={item.title + ' ' + '$' + item.formula + '$'}
                    />
                  </View>
                </Pressable>
              )}
              keyExtractor={(item) => item.formula}
            />
          </View>
        </View>

        <View style={styles.textarea_wrapper}>
          <View style={styles.input_toolbar_wrapper}>
            <TextInput
              multiline
              numberOfLines={1}
              style={styles.input}
              value={message}
              spellCheck={false}
              onSelectionChange={(event) =>
                setPosition(event.nativeEvent.selection.end)
              }
              onChangeText={(text) => {
                setMessage(text);
              }}
              placeholder="Enter a message"
            />
          </View>

          <View style={styles.back_button_wrapper}>
            <Pressable
              style={styles.backButton}
              onPress={() => {
                navigation.replace('Chat', {
                  firstUser: firstUser,
                  firstAvatar: firstAvatar,
                  firstMail: firstMail,
                  group: group,
                  chatroomId: chatroomId,
                  latex: message,
                });
              }}>
              <Text style={styles.back_text}>Back To Chat</Text>
            </Pressable>
          </View>
        </View>
      </KeyboardAvoidingView>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  wrapper: {
    width: '100%',
    height: '100%',
  },
  compiled_wrapper: {
    backgroundColor: 'white',
    height: '30%',
  },
  button_wrapper: {
    height: '55%',
  },
  input_toolbar_wrapper: {
    width: '85%',
  },
  back_button_wrapper: {
    width: '15%',
    alignSelf: 'flex-end',
  },
  compiled_text_wrapper: {
    height: '100%',
  },
  find_wrapper: {
    height: '100%',
    width: '80%',
  },
  list_find_text: {
    height: '100%',
    width: '100%',
    fontSize: 16,
  },
  find_button_wrapper: {
    height: '100%',
    width: '15%',
    alignSelf: 'flex-end',
    position: 'absolute',
    backgroundColor: '#99cfe0',
    borderRadius: 4,
    flex: 1,
    justifyContent: 'center', // Align items vertically in the middle
    alignItems: 'center', // Align items horizontally in the center
  },
  find_button: {
    width: '100%',
    height: '100%',
  },
  find_text: {
    justifyContent: 'center',
    position: 'absolute',
    top: '26%',
    left: '26%',
    fontSize: 18,
    alignSelf: 'center',
  },
  textarea_wrapper: {
    width: '100%',
    flex: 1,
    flexWrap: 'wrap',
    flexDirection: 'column',
    justifyContent: 'space-between',
    justifyContent: 'flex-end',
  },
  list_bg: {
    backgroundColor: '#2B768C',
    flex: 1,
  },
  list_item: {
    width: '100%',
    height: 55,
    alignContent: 'center',
    textAlign: 'center',
    borderRadius: 15,
    backgroundColor: '#97EDD9',
    padding: 5,
    borderWidth: 1,
  },
  list_item_title_1: {
    paddingBottom: 5,
    fontWeight: 400,
    fontSize: 17,
    backgroundColor: 'transparent',
    paddingLeft: 150,
  },
  list_item_title: {
    alignSelf: 'center',
    fontWeight: 400,
    paddingTop: 3,
    fontSize: 17,
  },
  formulas_search: {
    height: 50,
    width: '100%',
    flex: 0,
    borderWidth: 0.5,
  },

  input: {
    width: '100%',
    height: '100%',
    borderWidth: 0.5,
    fontSize: 16,
  },
  latexButton: {
    backgroundColor: 'blue',
    width: '60%',
    padding: 15,
    borderRadius: 5,
    alignItems: 'center',
    marginTop: 40,
    bottom: 20,
  },

  backButton: {
    height: '100%',
    backgroundColor: '#99cfe0',
    borderWidth: 0.5,
    borderRadius: 5,
  },
  back_text: {
    paddingLeft: 8,
    top: '20%',
    alignSelf: 'center',
    height: '100%',
    fontSize: 18,
  },
});

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
    packages: { '[+]': ['tagformat'] },
    extensions: [
      'AMSmath.js',
      'AMSsymbols.js',
      'noErrors.js',
      'noUndefined.js',
    ],
  },
};

export default LaTeXScreen;
