<?php 
session_start();
session_unset();
session_destroy();
header("location: /myshop/index.php");
exit();