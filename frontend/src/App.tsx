import { useState } from 'react'
import './App.css'
import { DiffViewer } from './components/DiffViewer';
import { Header } from './components/Header';
import { Sidebar } from './components/SideBar';
import CfgViewer from './components/CfgViewer';

function App() {
    const [viewMode, setViewMode] = useState<'IR' | 'CFG' | 'IR/CFG'>('IR'); { /* TODO: Add the IR/CFG later */}

    return (
        <div className="app-layout">
        <Header setViewMode={setViewMode} viewMode={viewMode} />
          <main className="main-layout">
              <Sidebar />
              {viewMode === 'IR' ? (
                  <DiffViewer />
              ) : (
                  <CfgViewer />
              )}
          </main>
        </div>
    )
}

export default App
