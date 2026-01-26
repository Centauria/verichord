#![doc = include_str!("../README.md")]
//! Library entry point for the verichord crate.
//!
//! This crate exposes installation helpers so other binaries (for example a separate
//! installer binary) can reuse the same install/uninstall logic programmatically.

pub mod install;

pub use install::{create_app_at, run_installer, uninstall_app_at};
