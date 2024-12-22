import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule } from '@angular/common/http';
import { HTTP_INTERCEPTORS } from '@angular/common/http';
import { AppComponent } from './app.component';
import { AdminMenuComponent } from './presentation/components/admin-menu/admin-menu.component';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';
import { PatientListComponent } from './presentation/components/patient-list/patient-list.component';
import { AppRoutingModule } from './app-routing.module';
import { FormsModule } from '@angular/forms';
import { RegisterUserComponent } from './presentation/components/register-user/register-user.component';
import { UpdateUserComponent } from './presentation/components/update-user/update-user.component';
import { DeleteUserComponent } from './presentation/components/delete-user/delete-user.component';
import { MainMenuComponent } from './presentation/components/main-menu/main-menu.component';
import { PatientMenuComponent } from './presentation/components/patient-menu/patient-menu.component';
import { RegisterOperationComponent } from './presentation/components/register-operation/register-operation.component';
import { provideAnimationsAsync } from '@angular/platform-browser/animations/async';
import {ListOperationtypeComponent} from './presentation/components/list-operationtype/list-operationtype.component';
import { JwtInterceptor } from './domain/services/jwt-interceptor.service';
import { LoginComponent } from './presentation/components/login/login.component';
import { WelcomeComponent } from './presentation/components/welcome/welcome.component';
import { LayoutComponent } from './presentation/components/layout/layout.component';


@NgModule({
  declarations: [
    AppComponent,
    PatientCreateComponent,  // Add the component here
    PatientUpdateComponent,
    PatientDeleteComponent,
    PatientListComponent,
    RegisterUserComponent,
    UpdateUserComponent,
    DeleteUserComponent,
    ListOperationtypeComponent,
    LoginComponent    
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    FormsModule,
    AppRoutingModule,
    RegisterOperationComponent
  ],
  providers: [
    provideAnimationsAsync(),
    {provide: HTTP_INTERCEPTORS, useClass: JwtInterceptor, multi: true}
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
