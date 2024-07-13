<?php 
include 'header.php';
if($_SESSION['type'] != "Customer") {
    header("location: /myshop/index.php");
    exit();
}
?>

<h2>My Orders</h2>

<div class="article-container">
    <?php
        if(isset($_GET['sort'])) {
            if($_GET['mode'] == 1) {
                $ord = "ASC";
            } else {
                $ord = "DESC";
            }
            $sql = "SELECT * FROM Ord NATURAL JOIN Product NATURAL JOIN Delivery WHERE reciever_id = '".$_SESSION['user']['customer_id']."' ORDER BY ".$_GET['sort']." $ord ;";
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
        <th><a href='/myshop/myorders.php?sort=order_id&mode=$next' >ID</a></th> 
        <th><a href='/myshop/myorders.php?sort=name&mode=$next' >PRODUCT</a></th> 
        <th><a href='/myshop/myorders.php?sort=quantity&mode=$next' >QUANTITY</a></th> 
        <th><a href='/myshop/myorders.php?sort=o_date&mode=$next' >DATE</a></th> 
      </tr>";
        if ($queryResults > 0) {
            while ($row = mysqli_fetch_assoc($result)) {
                include "order.php";
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