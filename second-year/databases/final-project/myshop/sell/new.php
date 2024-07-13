<?php
include '../header.php';
if($_SESSION['type'] != "Seller") {
    header("location /myshop/index.php");
}
?>

<section class="signup-form">
    <h2>Sell</h2> 
    <div class="signup-form-form">
        <form action= "/myshop/includes/sell_new.inc.php" method="post">
        <input type="text" name="name" placeholder="Product Name...">
        <br>
        <input type="text" name="description" placeholder="Description...">
        <br>
        <input type="text" name="category" placeholder="Category...">
        <br>
        <input type="number" name="price" placeholder="Price...">
        <br>
        <input type="number" name="available" placeholder="Quantity...">
        <br>
        <button type="submit" name="submit">Submit</button>
        </form>
    </div>
</section>
<?php
if (isset($_GET["error"])) {
    if ($_GET["error"] == "empty_input") {
    echo "<p>Fill in all fields!</p>";
    } else if ($_GET["error"] =="wrong_input") {
        echo "<p>Invalid Input!</p>";
    } 
}
?>


<?php 
include '/myshop/footer.php';
?>