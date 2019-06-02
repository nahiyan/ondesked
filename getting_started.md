---
description: >-
  By the end of this guide, you should be ready to craft apps with Ondesked.
---

# Getting Started

## Installing wxWidgets

Ondesked compiles to C++, which requires the GUI toolkit - wxWidgets - to work.

Make sure you install wxWidgets before you start using Ondesked. Here's the [official download page](https://www.wxwidgets.org/downloads/). Moreover, the official wiki features [guides for installation](https://wiki.wxwidgets.org/Install).

It is recommended to use package managers whereas possible. In macOS, you can use homebrew. Run the following command for the latest stable release:

```sh
brew install wxwidgets
```

You can alternatively run this command with the `HEAD` flag to install the latest development release:

```sh
brew install wxwidgets --HEAD
```

On Windows, you can use chocolatey to install wxWidgets:

```sh
choco install wxwidgets
```

For Linux, it's recommended to follow the official installation guides of wxWidgets.

## Installing Ondesked

## Compiling the Generated C++ App

Ondesked code will always compile to C++. We need to compile the C++ code with headers, libraries, etc of wxWidgets provided to the compiler.

The easiest way to do so is to use the program `wx-config` which should come with wxWidgets. `wx-config` will output the options and flags that needs to be passed to the compiler so you don't have to worry about it at all.

For example, this will output all the flags and 