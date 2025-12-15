;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — ada-loom-registry

(ecosystem
  (version "1.0.0")
  (name "ada-loom-registry")
  (type "project")
  (purpose "*Note:* This directory is named `ada-loom-registry` but contains a Haskell project called \"Spindle\" - *not an Ada project*.")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "*Note:* This directory is named `ada-loom-registry` but contains a Haskell project called \"Spindle\" - *not an Ada project*.")
  (what-this-is-not "- NOT exempt from RSR compliance"))
