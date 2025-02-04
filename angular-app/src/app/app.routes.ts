import { Routes } from '@angular/router';
import { LispComponent } from './Pages/lisp/lisp.component'
import { RuComponent } from './Pages/ru/ru.component'
import { AboutComponent } from './Pages/about/about.component'
import { HomeComponent } from './Pages/home/home.component'

export const routes: Routes = [
    { path: 'lisp', component: LispComponent },
    { path: 'ru', component: RuComponent },
    { path: 'about', component: AboutComponent },
    { path: '', component: HomeComponent },
    { path: '**', redirectTo: '', pathMatch: 'full' }
];
