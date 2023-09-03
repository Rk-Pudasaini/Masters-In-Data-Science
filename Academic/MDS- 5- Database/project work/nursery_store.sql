-- phpMyAdmin SQL Dump
-- version 5.2.1
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Generation Time: Jun 20, 2023 at 11:24 AM
-- Server version: 10.4.28-MariaDB
-- PHP Version: 8.0.28

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `nursery_store`
--

DELIMITER $$
--
-- Procedures
--
CREATE DEFINER=`root`@`localhost` PROCEDURE `CalculateTotalOrderAmount` (IN `orderDate` DATE, OUT `totalAmount` DECIMAL(10,2))   BEGIN
    SELECT SUM(total_amount) INTO totalAmount
    FROM orders
    WHERE order_date >= orderDate;
END$$

DELIMITER ;

-- --------------------------------------------------------

--
-- Table structure for table `categories`
--

CREATE TABLE `categories` (
  `category_id` int(11) NOT NULL,
  `name` varchar(100) DEFAULT NULL,
  `description` text DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `categories`
--

INSERT INTO `categories` (`category_id`, `name`, `description`) VALUES
(1, 'Flowers', 'Various types of beautiful flowering plants'),
(2, 'Foliage', 'Plants with lush green foliage'),
(3, 'Cacti', 'Low-maintenance desert plants'),
(4, 'Orchids', 'Exotic and delicate flowering plants'),
(5, 'Bonsai', 'Artfully pruned miniature trees'),
(6, 'Succulents', 'Drought-tolerant and unique plants'),
(7, 'Palms', 'Tropical indoor palm trees'),
(8, 'Air Plants', 'Epiphytic plants that absorb nutrients from the air'),
(9, 'Herbs', 'Plants used for culinary or medicinal purposes'),
(10, 'Ferns', 'Plants with delicate and feathery fronds');

-- --------------------------------------------------------

--
-- Table structure for table `customers`
--

CREATE TABLE `customers` (
  `customer_id` int(11) NOT NULL,
  `first_name` varchar(50) DEFAULT NULL,
  `last_name` varchar(50) DEFAULT NULL,
  `email` varchar(100) DEFAULT NULL,
  `phone` varchar(20) DEFAULT NULL,
  `address` varchar(200) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `customers`
--

INSERT INTO `customers` (`customer_id`, `first_name`, `last_name`, `email`, `phone`, `address`) VALUES
(1, 'John', 'Doe', 'johndoe@example.com', '9843342697', '123 Main Street'),
(2, 'Jane', 'Smith', 'janesmith@example.com', '9841778832', '456 Elm Avenue'),
(3, 'Michael', 'Johnson', 'michaeljohnson@example.com', '9861298978', '789 Oak Lane'),
(4, 'Emily', 'Brown', 'emilybrown@example.com', '9841667434', '987 Pine Road'),
(5, 'David', 'Miller', 'davidmiller@example.com', '9841667674', '654 Cedar Court'),
(6, 'Sarah', 'Wilson', 'sarahwilson@example.com', '98416674557', '321 Birch Lane'),
(7, 'Robert', 'Anderson', 'robertanderson@example.com', '9841567321', '456 Maple Street'),
(8, 'Jessica', 'Thomas', 'jessicathomas@example.com', '9854453467', '78 Sink Avenue'),
(9, 'Christopher', 'Jackson', 'christopherjackson@example.com', '9876846321', '987 Pine Lane'),
(10, 'Jennifer', 'White', 'jenniferwhite@example.com', '984672235', '654 Elm Court');

-- --------------------------------------------------------

--
-- Table structure for table `orders`
--

CREATE TABLE `orders` (
  `order_id` int(11) NOT NULL,
  `customer_id` int(11) DEFAULT NULL,
  `order_date` date DEFAULT NULL,
  `total_amount` decimal(10,2) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `orders`
--

INSERT INTO `orders` (`order_id`, `customer_id`, `order_date`, `total_amount`) VALUES
(1, 1, '2023-06-18', 75.99),
(2, 2, '2023-06-17', 42.50),
(3, 3, '2023-06-19', 115.75),
(4, 1, '2023-06-19', 32.99),
(5, 4, '2023-06-18', 50.00),
(6, 2, '2023-06-19', 89.95),
(7, 5, '2023-06-17', 27.50),
(8, 3, '2023-06-18', 64.75),
(9, 4, '2023-06-19', 75.00),
(10, 1, '2023-06-17', 112.50);

-- --------------------------------------------------------

--
-- Table structure for table `order_items`
--

CREATE TABLE `order_items` (
  `order_item_id` int(11) NOT NULL,
  `order_id` int(11) DEFAULT NULL,
  `plant_id` int(11) DEFAULT NULL,
  `quantity` int(11) DEFAULT NULL,
  `price` decimal(10,2) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `order_items`
--

INSERT INTO `order_items` (`order_item_id`, `order_id`, `plant_id`, `quantity`, `price`) VALUES
(1, 1, 2, 3, 38.97),
(2, 1, 5, 1, 24.99),
(3, 2, 3, 2, 19.98),
(4, 3, 1, 4, 79.96),
(5, 4, 4, 2, 15.98),
(6, 5, 2, 1, 12.99),
(7, 5, 6, 3, 119.97),
(8, 6, 7, 2, 13.98),
(9, 7, 3, 1, 9.99),
(10, 8, 1, 3, 59.97);

-- --------------------------------------------------------

--
-- Table structure for table `plants`
--

CREATE TABLE `plants` (
  `plant_id` int(11) NOT NULL,
  `name` varchar(100) DEFAULT NULL,
  `description` text DEFAULT NULL,
  `price` decimal(10,2) DEFAULT NULL,
  `quantity` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `plants`
--

INSERT INTO `plants` (`plant_id`, `name`, `description`, `price`, `quantity`) VALUES
(1, 'Rose', 'Beautiful flowering plant', 19.99, 10),
(2, 'Lily', 'Elegant and fragrant flower', 12.99, 5),
(3, 'Fern', 'Lush green foliage plant', 9.99, 20),
(4, 'Cactus', 'Low-maintenance desert plant', 7.99, 15),
(5, 'Orchid', 'Exotic and delicate flower', 24.99, 8),
(6, 'Bonsai', 'Artfully pruned miniature tree', 39.99, 3),
(7, 'Succulent', 'Drought-tolerant and cute', 6.99, 12),
(8, 'Palm', 'Tropical indoor palm tree', 29.99, 6),
(9, 'Snake Plant', 'Air-purifying and resilient', 14.99, 9),
(10, 'Pothos', 'Popular trailing houseplant', 8.99, 18);

--
-- Triggers `plants`
--
DELIMITER $$
CREATE TRIGGER `prevent_zero_qty` BEFORE INSERT ON `plants` FOR EACH ROW BEGIN
    IF NEW.quantity = 0 THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Quantity cannot be set to zero.';
    END IF;
END
$$
DELIMITER ;

-- --------------------------------------------------------

--
-- Table structure for table `plant_category`
--

CREATE TABLE `plant_category` (
  `plant_id` int(11) NOT NULL,
  `category_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

--
-- Dumping data for table `plant_category`
--

INSERT INTO `plant_category` (`plant_id`, `category_id`) VALUES
(1, 1),
(2, 1),
(3, 10),
(4, 3),
(5, 1),
(6, 5),
(7, 6),
(8, 7),
(9, 9),
(10, 9);

--
-- Indexes for dumped tables
--

--
-- Indexes for table `categories`
--
ALTER TABLE `categories`
  ADD PRIMARY KEY (`category_id`);

--
-- Indexes for table `customers`
--
ALTER TABLE `customers`
  ADD PRIMARY KEY (`customer_id`);

--
-- Indexes for table `orders`
--
ALTER TABLE `orders`
  ADD PRIMARY KEY (`order_id`),
  ADD KEY `customer_id` (`customer_id`);

--
-- Indexes for table `order_items`
--
ALTER TABLE `order_items`
  ADD PRIMARY KEY (`order_item_id`),
  ADD KEY `order_id` (`order_id`),
  ADD KEY `plant_id` (`plant_id`);

--
-- Indexes for table `plants`
--
ALTER TABLE `plants`
  ADD PRIMARY KEY (`plant_id`);

--
-- Indexes for table `plant_category`
--
ALTER TABLE `plant_category`
  ADD PRIMARY KEY (`plant_id`,`category_id`),
  ADD KEY `category_id` (`category_id`);

--
-- AUTO_INCREMENT for dumped tables
--

--
-- AUTO_INCREMENT for table `categories`
--
ALTER TABLE `categories`
  MODIFY `category_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=11;

--
-- AUTO_INCREMENT for table `customers`
--
ALTER TABLE `customers`
  MODIFY `customer_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=11;

--
-- AUTO_INCREMENT for table `orders`
--
ALTER TABLE `orders`
  MODIFY `order_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=11;

--
-- AUTO_INCREMENT for table `order_items`
--
ALTER TABLE `order_items`
  MODIFY `order_item_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=11;

--
-- AUTO_INCREMENT for table `plants`
--
ALTER TABLE `plants`
  MODIFY `plant_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=11;

--
-- Constraints for dumped tables
--

--
-- Constraints for table `orders`
--
ALTER TABLE `orders`
  ADD CONSTRAINT `orders_ibfk_1` FOREIGN KEY (`customer_id`) REFERENCES `customers` (`customer_id`);

--
-- Constraints for table `order_items`
--
ALTER TABLE `order_items`
  ADD CONSTRAINT `order_items_ibfk_1` FOREIGN KEY (`order_id`) REFERENCES `orders` (`order_id`),
  ADD CONSTRAINT `order_items_ibfk_2` FOREIGN KEY (`plant_id`) REFERENCES `plants` (`plant_id`);

--
-- Constraints for table `plant_category`
--
ALTER TABLE `plant_category`
  ADD CONSTRAINT `plant_category_ibfk_1` FOREIGN KEY (`plant_id`) REFERENCES `plants` (`plant_id`),
  ADD CONSTRAINT `plant_category_ibfk_2` FOREIGN KEY (`category_id`) REFERENCES `categories` (`category_id`);
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
