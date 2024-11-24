import { CommonModule } from '@angular/common';
import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-header-static',
  templateUrl: './header-static.component.html',
  styleUrls: ['./header-static.component.css'],
  standalone:true,
  encapsulation: ViewEncapsulation.None ,
  imports: [CommonModule]
})
export class HeaderStaticComponent implements OnInit{
  currentUrl: string | undefined;
  urlSegments: string[] = [];

  constructor(private router:Router) {}

  ngOnInit(): void {
    this.currentUrl = this.router.url;
    this.urlSegments = this.currentUrl.split('/').filter(segment => segment);
  }

  navigateTo(segment: string): void {
    const index = this.urlSegments.indexOf(segment);
    const path = this.urlSegments.slice(0, index + 1).join('/');
    this.router.navigate([`/${path}`]);
  }
  navigateToHome()
  {
    this.router.navigate(['/'])
  }

  
}

