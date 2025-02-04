import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router, NavigationEnd, NavigationStart } from '@angular/router';

@Component({
  selector: 'app-navbar',
  standalone: true,
  imports: [],
  templateUrl: './navbar.component.html',
  styleUrl: './navbar.component.scss'
})
export class NavbarComponent {
  title: string = 'Lisp Interpretor';

  constructor(private router: Router, private activatedRoute: ActivatedRoute) {}

  ngOnInit(): void {
    this.router.events.subscribe(event => {
      if (event instanceof NavigationEnd) {
        this.updateTitle(this.router.url);
      }
    });
  }

  updateTitle(url: string): void {
    console.log("update url: ", url);
    if (url.includes('/lisp')) {
      this.title = 'Lisp Interpretor';
    } else if (url.includes('/ru')) {
      this.title = 'Ru Interpretor';
    } else if (url.includes("/about")) {
      this.title = 'About';
    } else {
      this.title = 'Glados';
    }
  }

  redirect(path: string): void {
    this.router.navigate([path]);
  }
}
