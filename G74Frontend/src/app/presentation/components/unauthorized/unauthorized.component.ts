import { Component } from '@angular/core';

@Component({
  selector: 'app-unauthorized',
  template: `
    <div class="unauthorized">
      <h1>403 - Unauthorized</h1>
      <p>You do not have permission to access this page.</p>
    </div>
  `,
  styles: [
    `
      .unauthorized {
        text-align: center;
        margin-top: 50px;
      }
    `,
  ],
})
export class UnauthorizedComponent {}
