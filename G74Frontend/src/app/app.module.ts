import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule } from '@angular/common/http';
import { HTTP_INTERCEPTORS } from '@angular/common/http';
import { AppComponent } from './app.component';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';
import { PatientListComponent } from './presentation/components/patient-list/patient-list.component';
import { AppRoutingModule } from './app-routing.module';
import { FormsModule } from '@angular/forms';
import { RegisterUserComponent } from './presentation/components/register-user/register-user.component';
import { UpdateUserComponent } from './presentation/components/update-user/update-user.component';
import { DeleteUserComponent } from './presentation/components/delete-user/delete-user.component';
import { RegisterOperationComponent } from './presentation/components/register-operation/register-operation.component';
import { provideAnimationsAsync } from '@angular/platform-browser/animations/async';
import { ListOperationtypeComponent } from './presentation/components/list-operationtype/list-operationtype.component';
import { JwtInterceptor } from './domain/services/jwt-interceptor.service';
import { LoginComponent } from './presentation/components/login/login.component';
import { CreateMedicalConditionComponent } from './presentation/components/medical-condition-create/medical-condition-create.component';
import { MedicalConditionListComponent } from './presentation/components/medical-condition-list/medical-condition-list.component';
import { UpdateMedicalConditionComponent } from './presentation/components/medical-condition-update/medical-condition-update.component';
import { AllergyCreateComponent } from './presentation/components/allergy-create/allergy-create.component';
import { AllergyUpdateComponent } from './presentation/components/allergy-update/allergy-update.component';
import {AllergyListComponent} from './presentation/components/allergy-list/allergy-list.component';
import { ThreejsViewerComponent } from './presentation/components/threejs-viewer/threejs-viewer.component';
import { ListAllSpecializationComponent } from './presentation/components/list-all-specialization/list-all-specialization.component';
import { SpecializationCreateComponent } from './presentation/components/specialization-create/specialization-create.component';
import { AppointmentDashComponent } from './presentation/components/appointment-dash/appointment-dash.component';
import { AppointmentViewModel } from './application/viewmodels/appointment.viewmodel';


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
    LoginComponent,
    CreateMedicalConditionComponent,
    MedicalConditionListComponent,
    UpdateMedicalConditionComponent,
    AllergyCreateComponent,
    AllergyUpdateComponent,
    AllergyListComponent,
    ThreejsViewerComponent,
    AppointmentDashComponent,
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    FormsModule,
    AppRoutingModule,
    RegisterOperationComponent,
  ],
  providers: [
    provideAnimationsAsync(),
    { provide: HTTP_INTERCEPTORS, useClass: JwtInterceptor, multi: true }
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
