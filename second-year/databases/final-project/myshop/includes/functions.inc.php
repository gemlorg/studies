<?php

function get_id() {
    $next_id = rand(1, 10000000);
    return $next_id;
}
function empty_input($name, $surname,$username, $birth_date, $email, $adress, $password, $card, $type) {
    return empty($name) || empty($surname) || empty($birth_date) || empty($email) || empty($adress) || empty($password);
}
function find_user($conn, $uid, $email, $password, $type) {
    $sql = "SELECT * FROM `$type` WHERE email = '$email' OR ".$type."_id = '$uid'";
    
    $result = mysqli_query($conn, $sql);
    if ($row = mysqli_fetch_assoc($result)) {
        return $row;
    }else {
        $result = false;
        return $result;
    }
}
function wrong_data($conn, $name, $surname, $username, $birth_date, $email, $adress, $password, $card, $type) {
    $u = find_user($conn, $username, $email," ",$type);
    return $u != false;
}
function create_customer($conn, $name,$surname, $username, $birth_date, $email, $adress, $card,  $password, $type) {
    $hash = password_hash($password, PASSWORD_DEFAULT);
    $what = 'seller_id';

    if($type == "Customer") $what = 'customer_id';

    $sql = "INSERT INTO `$type`(`$what`, `name`, `surname`, `hash`, `adress`, `birth_date`, `email`, `card`) VALUES ('$username', '$name', '$surname', ?, '$adress', ?, '$email', ?);";    
    $stmt = $conn->prepare($sql);
    $stmt->bind_param("sss", $hash, $birth_date, $card);
    
    
    $stmt->execute();

    header ("location: ../index.php");
    exit();
}


function empty_input_login($email, $password) {
    return $email == NULL || $password == NULL;
}
function wrong_data_login($email, $password) {
    return false;
}
function login_user($conn, $email, $password, $type) {

    $user = find_user($conn, $email,$email, $password, $type);
    if($user == false) {
        header("location: /myshop/login/login.php?error=no_user");
        exit();
    }

    $password_check = password_verify($password, $user['hash']);
    if(!$password_check) {
        header("location: /myshop/login/login.php?error=wrong_password");
        exit();
    } else {
        session_start();
        $_SESSION["type"] = $type;
        $_SESSION["user"] = $user;
        header("location: /myshop/index.php");
        exit();
    }
}
function find_product($conn, $pid) {
    $sql = "SELECT * FROM `Product` WHERE product_id = $pid";
    
    $result = mysqli_query($conn, $sql);
    if ($row = mysqli_fetch_assoc($result)) {
        return $row;
    }else {
        $result = false;
        return $result;
    }
}

function get_deliveries($conn) {
    $sql = "SELECT * FROM Post";
    $result = mysqli_query($conn, $sql);    
    $queryResults = mysqli_num_rows($result) ;
        if ($queryResults > 0) {
            while ($row = mysqli_fetch_assoc($result)) {
                echo "<option name='".$row['name']."'>".$row['name']."</option>";
            }
        }
    
}

function check_quantity($conn, $pid, $quantity) {
    $pr = find_product($conn, $pid);

    return !($pr == false) && $quantity <= $pr['available'];
}
function create_payment($conn, $sid, $cid, $val) {
    $pid = get_id();
    $currentDate = new DateTime();
    
    $sql = "INSERT INTO `Payment` (`payment_id`, `p_date`, `amount`, `seller_id`, `customer_id`) VALUES (?,  ?, ?, ?, ?); ";
    $stmt = $conn->prepare($sql);
    $stmt->bind_param("ssiss", $pid, $currentDate->format('Y-m-d'), $val, $sid, $cid);
    $stmt->execute();
    return $pid;
}
function create_delivery($conn, $sid, $cid, $post) {
    $did = get_id();
    $currentDate = new DateTime();
    
    $sql = "INSERT INTO `Delivery` (`delivery_id`, `d_date`, `post_id`, `sender_id`, `reciever_id`) VALUES (?, ?, ?, ?, ?);";
    $stmt = $conn->prepare($sql);
    $stmt->bind_param("sssss", $did, $currentDate->format('Y-m-d'), $post, $sid, $cid);
    $stmt->execute();
    return $did;
}

function create_order($conn, $did, $pid, $quantity) {
    $oid = get_id();
    $currentDate = new DateTime();
    
    $sql = "INSERT INTO Ord (`order_id`, `o_date`, `delivery_id`, `product_id`, `quantity`) VALUES (?, ?, ?, ?, ?);";
    $stmt = $conn->prepare($sql);
    $stmt->bind_param("ssssi", $oid, $currentDate->format('Y-m-d'), $did, $pid, $quantity);
    $stmt->execute();
    return $oid;
}
function create_transaction($conn, $payment_id, $oid) {
    $tid = get_id();
    $currentDate = new DateTime();
    $sql = "INSERT INTO `Transaction` (`transcation_id`, `t_date`, `payment_id`, `order_id`) VALUES (?, ?, ?, ?);";
    $stmt = $conn->prepare($sql);
    $stmt->bind_param("ssss", $tid, $currentDate->format('Y-m-d'), $payment_id, $oid);
    $stmt->execute();
    return $oid;
}

function buy($conn, $pid, $quantity, $cid,  $post) {
    $product = find_product($conn, $pid);
    $sid = $product['seller_id'];
    $val = $product['price'] * $quantity;
    $payment_id = create_payment($conn, $sid, $cid, $val);
    $did = create_delivery($conn, $sid, $cid, $post);
    $oid = create_order($conn, $did, $pid, $quantity);
    $tid = create_transaction($conn, $payment_id, $oid);

    $sql = "UPDATE `Product` SET `available` = '".$product['available'] - $quantity."' WHERE `Product`.`product_id` = $pid;";
    $stmt = $conn->prepare($sql);
    $stmt->execute();
    
}
function check_category($conn, $category) {
    $sql = "SELECT * FROM `Category` WHERE name = '$category'";
    
    
    $result = mysqli_query($conn, $sql);
    if ($row = mysqli_fetch_assoc($result)) {
        return $row['category_id'];
    }else {
        $id = get_id();
        
        $sql = "INSERT INTO `Category` (`category_id`, `name`) VALUES ('$id', '$category');";
        $stmt = $conn->prepare($sql);
        $stmt->execute();
        
        return  $id;
    }

}
function sell_new($conn, $name, $description, $category, $available, $price, $sid) {
    
    $category_id = check_category($conn, $category);
    $product_id = get_id();
    $sql = "SELECT * FROM `Product` WHERE name = '$name' and seller_id = '$sid' and category_id = '$category_id'";
    $result = mysqli_query($conn, $sql);
    if ($row = mysqli_fetch_assoc($result)) {
        $new = $available + $row['available'];
        $sql = "UPDATE `Product` SET available ='$new'  WHERE name = '$name' and seller_id = '$sid' and category_id = '$category_id'";
    }else {
        $sql = "INSERT INTO `Product`(`product_id`, `name`, `available`, `description`, `price`, `category_id`, `seller_id`) VALUES ('$product_id','$name','$available','$description','$price','$category_id','$sid');";
        
        
    }
    $stmt = $conn->prepare($sql);
    $stmt->execute();

}

function check_product_name($conn, $name) {
    $sql = "SELECT * FROM `Product` WHERE name = '$name'";
    
    
    $result = mysqli_query($conn, $sql);
    if ($row = mysqli_fetch_assoc($result)) {
        return false;
    }else {
        return true;
    }

}

function change($conn, $pid, $name, $description, $available) {
    $sql = "UPDATE `Product` SET `name` = '$name', `available` = '$available', `description` = '$description' WHERE `Product`.`product_id` = '$pid'";
    $stmt = $conn->prepare($sql);
    $stmt->execute();
}
