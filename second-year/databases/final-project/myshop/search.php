<?php
include 'header.php'
?>
<h1> Search Page</h1>
<div class="article-container">
    <?php
        if (isset($_POST['submit-search'])) {
            $search = mysqli_real_escape_string($conn, $_POST['search']);
            $sort = mysqli_real_escape_string($conn, $_POST['sort']);
            $order = mysqli_real_escape_string($conn, $_POST['sort_type']);
            $range = mysqli_real_escape_string($conn, $_POST['price']);

            if($range == 'ALL') {

                $a = 0;
                $b = 1000000000;
            }else
            if($range == '0-500') {
                $a = 0;
                $b = 500;
            } else if ($range == '500-1000') {
                $a = 500;
                $b = 1000;
            }else if ($range == '1000-2000') {
                $a = 1000;
                $b = 2000;
            }else {
                $a = 2000;
                $b = 100000000;
            }
            if($order == "Ascending") {
                $order = "ASC";
            }else {
                $order = "DESC";
            }
          

            $sql = "SELECT  Product.name AS name,  Product.product_id as product_id,  Product.description AS description , Product.price AS price, Category.name AS category FROM Product LEFT JOIN Category ON Product.category_id = Category.category_id WHERE (Product.name LIKE '%$search%' OR Product.description LIKE '%$search%') AND Product.price >= $a AND Product.price <= $b ORDER BY $sort $order";
            $result = mysqli_query($conn, $sql);
            $queryResults = mysqli_num_rows($result) ;
            if ($queryResults > 0) {
                while ($row = mysqli_fetch_assoc($result)) {
                    include "item.php";
                }
        }
    }
    ?>
</div>

<?php 
include 'footer.php';
?>
