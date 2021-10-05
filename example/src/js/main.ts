/*
 * Copyright (C) 2021  Jean-Michel Bea
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import "./global.css";
import "./style.scss";
import { createInertiaApp } from "@inertiajs/inertia-svelte";
import { InertiaProgress } from "@inertiajs/progress";

InertiaProgress.init();

//@ts-ignore
const pages = import.meta.glob("./Pages/**/*.svelte");

createInertiaApp({
  resolve: (name: string) => {
    const importPage = pages[`./Pages/${name}.svelte`];
    if (!importPage) {
      throw new Error(
        `Unknown page ${name}. Is it located under Pages with a .svelte extension?`
      );
    }
    return importPage();
  },
  setup({ el, App, props }) {
    new App({ target: el, props });
  },
});
