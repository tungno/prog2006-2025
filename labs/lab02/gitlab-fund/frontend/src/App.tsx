import { BrowserRouter as Router, Routes, Route, Link } from 'react-router-dom';
import { FunderDashboard } from './components/Funder/FunderDashboard';
import { DeveloperDashboard } from './components/Developer/DeveloperDashboard';
import { ValidatorDashboard } from './components/Validator/ValidatorDashboard';
import { Home } from './components/Home/Home';
import './App.css';
import { DiagnosticComponent} from "./components/diagostic/DiagonosticComponent";

function App() {
    return (
        <Router>
            <div className="min-h-screen bg-gray-100">
                <nav className="bg-white shadow-lg">
                    <div className="max-w-7xl mx-auto px-4">
                        <div className="flex justify-between h-16">
                            <div className="flex space-x-4 items-center">
                                <Link to="/" className="text-gray-800 font-bold">GitLab Fund</Link>
                                <Link to="/funder" className="text-gray-600 hover:text-gray-800">Funder</Link>
                                <Link to="/developer" className="text-gray-600 hover:text-gray-800">Developer</Link>
                                <Link to="/validator" className="text-gray-600 hover:text-gray-800">Validator</Link>
                                <Link to="/diagnostic" className="text-gray-600 hover:text-gray-800">Diagnostic</Link>
                            </div>
                        </div>
                    </div>
                </nav>

                <div className="max-w-7xl mx-auto px-4 py-6">
                    <Routes>
                        <Route path="/funder" element={<FunderDashboard />} />
                        <Route path="/developer" element={<DeveloperDashboard />} />
                        <Route path="/validator" element={<ValidatorDashboard />} />
                        <Route path="/" element={<Home />} />
                        <Route path="/diagnostic" element={<DiagnosticComponent />} />
                    </Routes>
                </div>
            </div>
        </Router>
    );
}

export default App;