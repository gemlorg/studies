import { Button, useMediaQuery } from '@mui/material';
import { groceryContext } from '../../Layout/Layout';
import { useContext } from 'react';
import { checkoutContext } from '../Cart';

const OrderSummary = () => {
    // Get Cart Items from Context
    const { cartItemsState } = useContext(groceryContext);
    const [cartItems, setCartItems] = cartItemsState;
    const [isProceedToCheckout, setIsProceedToCheckout] = useContext(checkoutContext);

    // Media Query
    const isMediumScreen = useMediaQuery('(max-width:1024px)');

    const subtotal = Number.parseFloat(cartItems.reduce((total, item) => total + Number.parseFloat(item.total), 0));

    return (
        <div className='flex justify-center md:pt-16 col md:col-span-4 lg:col-span-1'>
            <div className={`lg:space-y-4 sticky top-0 bottom-0 w-full max-w-[25rem] space-y-3`}>
                {/* Title */}
                <h3 className='lg:text-xl text-lg sm:font-semibold font-bold tracking-wide'>Podsumowanie</h3>

                {/* Total Bill */}
                <table className='table-auto h-28 text-sm w-full'>
                    <tbody>
                        {/* Total */}
                        <tr className='lg:font-medium font-semibold lg:text-lg'>
                            <td>Łącznie</td>
                            <td style={{ color: 'green' }}>{(subtotal).toFixed(2)} zł</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>
    );
};

export default OrderSummary;