import React from 'react';
import './Home.css';

export const Home: React.FC = () => {
    return (
        <div className="text-center mt-10">
            <h1 className="text-4xl font-bold text-black">Welcome to GitLab Fund</h1>
            <p className="mt-4 text-gray-600">Choose your role to get started</p>
        </div>
    );
};