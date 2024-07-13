<?php 
include 'header.php';
if($_SESSION['type'] != "Seller") {
    header("location: /myshop/index.php");
    exit();
}
?>

<h2>My Products</h2>

<div class="article-container">
    <?php
        if(isset($_GET['sort'])) {

            if($_GET['mode'] == 1) {
                $ord = "ASC";
            } else {
                $ord = "DESC";
            }
            $sql = "SELECT * FROM Product WHERE seller_id = '".$_SESSION['user']['seller_id']."' ORDER BY ".$_GET['sort']." $ord ;";
        } else {
            $sql = "SELECT * FROM Product WHERE seller_id = '".$_SESSION['user']['seller_id']."';";
        }
        $result = mysqli_query($conn, $sql);
        $queryResults = mysqli_num_rows($result) ;
        if (isset($_GET['sort'])) {
            $next = 3 - (int)$_GET['mode'];
        } else {
            $next = 1;
        }

        echo "<table>";
        echo "<tr>
        <th><a href='/myshop/my_products.php?sort=name&mode=$next' >PRODUCT</a></th> 
        <th><a href='/myshop/my_products.php?sort=available&mode=$next' >QUANTITY</a></th>
        <th><a href='/myshop/my_products.php?sort=price&mode=$next' >PRICE</a></th>
        <th> CUSTOMIZE </th>
      </tr>";
        if ($queryResults > 0) {
            while ($row = mysqli_fetch_assoc($result)) {
                include "my_item.php";
            }
        }
        echo "</table>"
    ?>
</div>
<?php

?>


<?php 
include 'footer.php';
?>