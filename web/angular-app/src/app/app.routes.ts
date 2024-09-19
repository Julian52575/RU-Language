import { Routes } from '@angular/router';
import { LispComponent } from './Pages/lisp/lisp.component'
import { AboutComponent } from './Pages/about/about.component'
import { HomeComponent } from './Pages/home/home.component'

export const routes: Routes = [
    {path:'lisp', component: LispComponent},
    {path:'about', component: AboutComponent},
    {path:'', component: HomeComponent},
];
