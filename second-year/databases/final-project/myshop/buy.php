<?php 
include 'header.php';
require_once 'includes/functions.inc.php'
?>
<?php
if($_SESSION["type"] != "Customer") {
    header("location: /myshop/index.php?error=not_customer");
    exit();
}
$product = find_product($conn, $_GET['product_id']);
echo "<h1>".$product['name']."</h1>";
echo "<h2> Price: ".$product['price']."</h2>";
?>
<section class="buy-form">
    <h2>Checkout: </h2>
    <div class="buy-form-form">
        <?php
        $id = $_GET['product_id'];
       echo  '<form action= "/myshop/includes/buy.inc.php?product_id='.$id.'" method="post">';
        ?>
        <input type="number" name="quantity" placeholder="Quantity">
            <select name="delivery">
                <?php
                get_deliveries($conn);
                ?> 
                
            </select>
            <button type="submit" name="submit">Buy</button>
        </form>
    </div>
</section>

<?php 
include 'footer.php';
?>