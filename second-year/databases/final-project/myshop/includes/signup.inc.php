<?php
if (isset($_POST["submit" ])){
    
    require_once 'dbh.inc.php';
    require_once 'functions.inc.php';
    $username = $_POST["username"];
    $name = $_POST["name"];
    $surname = $_POST["surname"];
    $birth_date = $_POST["brith_date"];
    $email = $_POST["email"];
    $adress = $_POST["adress"];
    $password = $_POST["password"];
    $card = $_POST["card"];
    $type = $_POST["type"];
    
    //check if data is correct
    if(empty_input($name, $surname,$username, $birth_date, $email, $adress, $password, $card, $type)) {
        header("location: /myshop/login/signup.php?error=empty_input");
        exit();
    }
    if(wrong_data($conn, $name, $surname, $username, $birth_date, $email, $adress, $password, $card, $type)) {
        header("location: /myshop/login/signup.php?error=wrong_input");
        exit();
    }
    
    
    
    create_customer($conn, $name, $surname, $username, $birth_date, $email, $adress, $card,  $password, $type);



} else {
    header("location: /myshop/login/signup.php");
}