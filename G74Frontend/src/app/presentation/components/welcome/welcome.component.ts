import { Component, ViewEncapsulation } from '@angular/core';
import { HeaderStaticComponent } from '../header-static/header-static.component';
import { AuthService } from '../../../domain/services/auth.service';
@Component({
  selector: 'app-welcome',
  templateUrl: './welcome.component.html',
  styleUrl: './welcome.component.css',
  encapsulation: ViewEncapsulation.None,
  standalone: true,
  imports: [HeaderStaticComponent]
})
export class WelcomeComponent {
  user:any;
  constructor (private authService: AuthService) {
    
  }
  ngOnInit() {
    this.user = this.authService.currentUserSubject.value;
    console.log(this.user.role);
  }
}
