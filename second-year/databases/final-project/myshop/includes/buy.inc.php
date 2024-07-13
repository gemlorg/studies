<?php
session_start();

if (isset($_POST["submit" ])){

    require_once 'dbh.inc.php';
    require_once 'functions.inc.php';
    $pid = $_GET["product_id"];
    echo $pid;
    $quantity = $_POST["quantity"];
    $post = $_POST["delivery"];
    //check quantity >0 and other stuff
    if($quantity <= 0 ) {
        header("location: /myshop/buy.php?error=wrong_input&product_id=$pid");
        exit();
    }
    if(!check_quantity($conn, $pid, $quantity)){
        header("location: /myshop/buy.php?error=wrong_quantity&product_id=$pid");
        exit();
    }

    buy($conn, $pid, $quantity, $_SESSION['user']['customer_id'], $post);
    header("location: /myshop/index.php?order=done");
    exit();


} else {
    header("location: /myshop/index.php");
}