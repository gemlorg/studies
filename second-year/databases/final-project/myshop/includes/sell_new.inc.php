<?php
session_start();

if (isset($_POST["submit" ])){
    require_once 'dbh.inc.php';
    require_once 'functions.inc.php';
    $name = $_POST['name'];
    $description = $_POST['description'];
    $category = $_POST['category'];
    $available = $_POST['available'];
    $price = $_POST['price'];

    




    
    //check quantity >0 and other stuff
    if($available <= 0 || $price < 0 ) {
        header("location: /myshop/sell/new.php?error=wrong_input");
        exit();
    }
    // if(!check_product_name($conn, $name)) {
    //     header("location: /myshop/sell/new.php?error=wrong_input");
    //     exit();
    // }
    
    sell_new($conn, $name, $description, $category, $available, $price, $_SESSION['user']['seller_id']);
    header("location: /myshop/index.php?sell=done");
    exit();


} else {
    header("location: /myshop/index.php");
}