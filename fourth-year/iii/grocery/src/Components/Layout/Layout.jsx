import { Outlet } from "react-router-dom";
import Navbar, { userLoggedIn } from '../Navbar/Navbar';
import { createContext, useState } from "react";
import { handleSessionStorage } from "../../utils/utils";

export const groceryContext = createContext();
const Layout = () => {
    const [isUserLoggedIn, setIsUserLoggedIn] = useState(userLoggedIn);
    const [cartItems, setCartItems] = useState(cartItemsFromSessionStorage);

    return (
        <groceryContext.Provider value={{
            userLoggedInState: [isUserLoggedIn, setIsUserLoggedIn],
            cartItemsState: [cartItems, setCartItems]
        }}>
            <Navbar />
            <section className="min-h-screen">
                <Outlet />
            </section>
        </groceryContext.Provider>
    );
};

// Get CartItems from SessionStorage
const cartItemsFromSessionStorage = handleSessionStorage('get', 'cartItems') || [];

export default Layout;