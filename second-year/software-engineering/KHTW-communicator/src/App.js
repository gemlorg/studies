import { StatusBar } from "expo-status-bar";
import { StyleSheet, Text, View } from "react-native";
import { NavigationContainer } from "@react-navigation/native";
import { createNativeStackNavigator } from "@react-navigation/native-stack";
import LoginScreen from "./screens/LoginScreen";
import HomeScreen from "./screens/HomeScreen";
import ChatScreen from "./screens/ChatScreen";
import LaTeXScreen from './screens/LaTeXScreen';


const Stack = createNativeStackNavigator();

export default function App() {
        return (
                <NavigationContainer>
                        <Stack.Navigator>
                                <Stack.Screen name="Login" component={LoginScreen} />
                                <Stack.Screen name="Home" component={HomeScreen} />
                                <Stack.Screen name="Chat" component={ChatScreen} />
                                <Stack.Screen name="LaTeX" component={LaTeXScreen} />

                        </Stack.Navigator>
                </NavigationContainer>
        );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: "#fff",
    alignItems: "center",
    justifyContent: "center",
  },
});
