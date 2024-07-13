<?php
session_start();

if (isset($_POST["submit" ])){

    require_once 'dbh.inc.php';
    require_once 'functions.inc.php';
    $pid = $_GET["product_id"];
    $name = $_POST["name"];
    $description = $_POST["description"];
    $available = $_POST["available"];
    echo $name;
    
    if($available < 0 || $name == "") {
        header("location: /myshop/my_products.php?error=wrong_input&product_id=$pid");
        exit();
    }
    

    // buy($conn, $pid, $quantity, $_SESSION['user']['customer_id'], $post);
    change($conn, $pid, $name, $description, $available);
    header("location: /myshop/my_products.php");
    exit();


} else {
    header("location: /myshop/index.php");
}