import { Component } from '@angular/core';

@Component({
  selector: 'app-lisp',
  standalone: true,
  imports: [],
  templateUrl: './lisp.component.html',
  styleUrl: './lisp.component.scss'
})
export class LispComponent {
  public run() {
    console.log('Running Lisp code');
  }
}
