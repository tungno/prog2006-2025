pub mod app;
pub mod simulation_view;
pub mod controls;
pub mod heuristic_editor;
pub mod statistics_view;
pub mod styles;

use std::time::Instant;
use std::collections::HashMap;

pub use simulation_view::HeatmapType;
pub use styles::*;
use crate::heuristics::StrategyParameters;
use crate::statistics::StatisticType;

/// UI Messages
#[derive(Debug, Clone)]
pub enum Message {
    // Simulation control
    Tick(Instant),
    ToggleRunning,
    Reset,
    SetTickSpeed(u64),
    
    // Navigation
    SetActivePage(Page),
    
    // Simulation interaction
    CellClicked(usize, usize),
    SetFoodRegenerationRate(f32),
    SetWaterRegenerationRate(f32),
    ToggleShowSensingRadius,
    ToggleShowAgentPaths,
    ToggleShowHeatmap,
    SetHeatmapType(HeatmapType),
    TriggerEvolution,
    
    // Agent tracking
    TrackAgent(String),
    ClearTrackedAgents,
    
    // Heuristic editor
    CreateStrategy(String),
    DeleteStrategy(String),
    EditStrategy(String),
    UpdateStrategy(StrategyParameters),
    CancelStrategyEdit,
    ImportStrategies(HashMap<String, StrategyParameters>),
    ExportStrategies,
    
    // Statistics
    SelectStatisticType(StatisticType),
}

/// Application pages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Page {
    Simulation,
    HeuristicEditor,
    Statistics,
}