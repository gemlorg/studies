<?php

if (isset($_POST["submit" ])){

    require_once 'dbh.inc.php';
    require_once 'functions.inc.php';
    $email = $_POST["email"];
    $password = $_POST["password"];
    $type = $_POST["type"];

    //check if data is correct
    if(empty_input_login($email, $password)) {
        header("location: /myshop/login/login.php?error=empty_input");
        exit();
    }
    if(wrong_data_login($email, $password)) {
        header("location: /myshop/login/login.php?error=wrong_input");
        exit();
    }

    
    login_user($conn, $email, $password, $type);



} else {
    header("location: /myshop/login/login.php");
}