// Import the functions you need from the SDKs you need
//
// import * as firebase from "firebase";
// import * as firebase from "firebase/compact";
import firebase from "firebase/compat/app";
import { getAuth } from "firebase/auth"
import 'firebase/compat/database';//import 'firebase/auth';

// TODO: Add SDKs for Firebase products that you want to use
// https://firebase.google.com/docs/web/setup#available-libraries

// Your web app's Firebase configuration
const firebaseConfig = {
    apiKey: "AIzaSyDk2UJip47cwijaKAM7JKsloO-4OiUo3G8",
    authDomain: "khtw-communicator.firebaseapp.com",
    databaseURL: "https://khtw-communicator-default-rtdb.firebaseio.com/",
    projectId: "khtw-communicator",
    storageBucket: "khtw-communicator.appspot.com",
    messagingSenderId: "620609946458",
    appId: "1:620609946458:web:c3ff051d15a474f8462963"
};

// Initialize Firebase
// let app;
// if (firebase.app.length === 0) {
//     app = firebase.initializeApp(firebaseConfig);
// } else {
//     app = firebase.app();
// }

const app = firebase.initializeApp(firebaseConfig);

const auth = getAuth(app);

const database = firebase.database();

const findUserByMail = async name => {
    const ref = database.ref("users");
    var query = ref.orderByChild("mail").equalTo(name);
    const snapshot = await query.once("value");
    const users = [];
    snapshot.forEach(function(child) {
        users.push(child.val());
    });
    return users;
};

export { auth, findUserByMail };
