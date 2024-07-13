<?php 
include 'header.php';
?>

<h1> Front Page </h1>
<?php
if (isset($_GET["error"])) {
    if ($_GET["error"] == "not_customer") {
    echo "<p>Log into customer account to buy!</p>";
    }
}else if(isset($_GET["order"])) {
    if ($_GET["order"] == "done") {
        echo "<p>Your order was successful!</p>";
        }
}
?>
<h2>All Items</h2>

<div class="article-container">
    <?php
        if(isset($_GET["category"])) {
            $sql = "SELECT Product.name, Product.product_id as product_id,  Product.description, Product.price, Category.name AS category FROM Product LEFT JOIN Category ON Product.category_id = Category.category_id WHERE Category.name LIKE '".$_GET["category"]."'";
        }else {
        
            $sql = "SELECT Product.name as name, Product.product_id as product_id, Product.description as description, Product.price, Category.name AS category FROM Product LEFT JOIN Category ON Product.category_id = Category.category_id";
        }
        $result = mysqli_query($conn, $sql);
        
        $queryResults = mysqli_num_rows($result) ;
        if ($queryResults > 0) {
            while ($row = mysqli_fetch_assoc($result)) {

                include "item.php";
            }
        }
    ?>
</div>

<?php 
include 'footer.php';
?>

