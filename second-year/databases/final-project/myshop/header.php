<?php
session_start();
include "includes/dbh.inc.php";
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <!-- <title>Main Page</title> -->
    <link href="/myshop/style.css" rel="stylesheet" type="text/css" >
</head>
<body>
<nav>
<div class="wrapper">
<a href="index.php"><img src="img/logo-white.png" alt="Blogs logo"></a>
<ul> 
    <li><a href="/myshop/index.php">Home</a></li>  
    <?php
    if($_SESSION["type"] == "Seller") {
        echo "<li><a href='/myshop/sell/new.php'>Sell</a></li>";
        echo "<li><a href='/myshop/my_products.php' >My Products</a></li>";
        echo "<li><a href='/myshop/my_sells.php' >Sold</a></li>";
        echo "<li><a href='/myshop/logout.php' >Logout</a></li>";
    }else if($_SESSION["type"] == "Customer") {
        echo "<li><a href='/myshop/myorders.php' >My Orders</a></li>";
        echo "<li><a href='/myshop/logout.php' >Logout</a></li>";
    } else {
        echo "<li><a href='/myshop/login/signup.php' >Sign up</a></li>";
        echo "<li><a href='/myshop/login/login.php' >Log in</a></li> ";
    }
    ?>
    
</ul>
</div>
</nav>
<form action="search.php" method= "POST"> 
    <input type="text" name="search" placeholder= "Search">
    <select name="sort" >
        <option name="price">Price</option>
        <option name="name">Name</option>
        <option name="category">Category</option>
    </select>
    <select name="sort_type" >
        <option name="ascd">Ascending</option>
        <option name="desc">Descending</option>
    </select>
    <select name="price" >
    <option name="aa">ALL</option>
        <option name="a">0-500</option>
        <option name="b">500-1000</option>
        <option name="c">1000-2000</option>
        <option name="d">2000+</option>
    </select>
    <button type="submit" name="submit-search">Search</button>
</form>