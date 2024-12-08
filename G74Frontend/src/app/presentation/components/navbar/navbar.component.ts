import { CommonModule } from '@angular/common';
import { Component, ElementRef, Renderer2 } from '@angular/core';
import { AuthService } from '../../../domain/services/auth.service';
import { Router } from '@angular/router';


@Component({
  selector: 'app-navbar',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './navbar.component.html',
  styleUrl: './navbar.component.css'
})
export class NavbarComponent {
  user: any;
  constructor (private authService: AuthService,private renderer: Renderer2, private el: ElementRef,private router: Router) {}
  
  ngOnInit() {
    this.user = this.authService.currentUserSubject.value;
    console.log(this.user);
  }
  ngAfterViewInit() {
    const navbar = this.el.nativeElement.querySelector('#navbar');
    const resizeHandle = this.el.nativeElement.querySelector('#resize-handle');

    let isResizing = false;

    this.renderer.listen(resizeHandle, 'mousedown', (event: MouseEvent) => {
      isResizing = true;
      event.preventDefault();
    });

    this.renderer.listen(document, 'mousemove', (event: MouseEvent) => {
      if (isResizing) {
        const newWidth = event.clientX - navbar.getBoundingClientRect().left;
        this.renderer.setStyle(navbar, 'width', `${newWidth}px`);
      }
    });

    this.renderer.listen(document, 'mouseup', () => {
      isResizing = false;
    });
  }

  navigateToOperations(){
    this.router.navigate(["/doctor/list-operation/"]);
  }

  navigateToPatientMenu(){
    this.router.navigate(["/patient/"]);
  }

  navigateToHome(){
    this.router.navigate(["/home"]);
  }


}
