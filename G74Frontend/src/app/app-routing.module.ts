import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';
import { PatientListComponent } from './presentation/components/patient-list/patient-list.component';
import { StaffCreateComponent } from './presentation/components/staff-create/staff-create.component';
import { StaffUpdateComponent } from './presentation/components/staff-update/staff-update.component';
import { ListAllStaffComponent } from './presentation/components/list-all-staff/list-all-staff.component';
import { SpecializationCreateComponent } from './presentation/components/specialization-create/specialization-create.component';
import { ListAllSpecializationComponent } from './presentation/components/list-all-specialization/list-all-specialization.component';
import { AdminMenuComponent } from './presentation/components/admin-menu/admin-menu.component';
import { RegisterUserComponent } from './presentation/components/register-user/register-user.component';
import { UpdateUserComponent } from './presentation/components/update-user/update-user.component';
import { DeleteUserComponent } from './presentation/components/delete-user/delete-user.component';
import { MainMenuComponent } from './presentation/components/main-menu/main-menu.component';
import { PatientMenuComponent } from './presentation/components/patient-menu/patient-menu.component';
import { DoctorMenuComponent } from './presentation/components/doctor-menu/doctor-menu.component';
import { RegisterOperationComponent } from './presentation/components/register-operation/register-operation.component';
import { UpdateOperationComponent } from './presentation/components/update-operation/update-operation.component';
import { ListAllOperationComponent } from './presentation/components/list-all-operation/list-all-operation.component';
import { DeleteOperationComponent } from './presentation/components/delete-operation/delete-operation.component';
import {ListOperationtypeComponent} from './presentation/components/list-operationtype/list-operationtype.component';
import { LoginComponent } from './presentation/components/login/login.component';
import { AuthGuard } from './domain/services/auth-guard.service';
import { UnauthorizedComponent } from './presentation/components/unauthorized/unauthorized.component';
import { WelcomeComponent } from './presentation/components/welcome/welcome.component';
import { LayoutComponent } from './presentation/components/layout/layout.component';

import { CreateMedicalConditionComponent } from './presentation/components/medical-condition-create/medical-condition-create.component';
import { UpdateMedicalConditionComponent } from './presentation/components/medical-condition-update/medical-condition-update.component';
import { MedicalConditionListComponent} from './presentation/components/medical-condition-list/medical-condition-list.component'
import {AllergyCreateComponent} from './presentation/components/allergy-create/allergy-create.component';
import {AllergyUpdateComponent} from './presentation/components/allergy-update/allergy-update.component';
import {AllergyListComponent} from './presentation/components/allergy-list/allergy-list.component';
import { MedicalRecordDashComponent } from './presentation/components/medical-record-dash/medical-record-dash.component';
import { ThreejsViewerComponent } from './presentation/components/threejs-viewer/threejs-viewer.component';
import { AppointmentDashComponent } from './presentation/components/appointment-dash/appointment-dash.component';

const routes: Routes = [
  { path: '', redirectTo: '/main', pathMatch: 'full' },
  { path: 'main', component: MainMenuComponent },
  { path: 'login', component: LoginComponent},


  { path: '', component : LayoutComponent,
    children: [
      { path: '', redirectTo: 'welcome', pathMatch: 'full' },
      { path: 'welcome', component: WelcomeComponent },

      //Admin routes
      { path: 'register-user', component: RegisterUserComponent },
      { path: 'admin/create-patient', component: PatientCreateComponent },
      { path: 'admin/update-patient', component: PatientUpdateComponent },
      { path: 'admin/delete-patient', component: PatientDeleteComponent },
      { path: 'admin/list-patient', component: PatientListComponent },
      { path: 'admin/register-user', component: RegisterUserComponent },
      { path: 'admin/create-staff', component: StaffCreateComponent },
      { path: 'admin/list-all-staff', component: ListAllStaffComponent },
      { path: 'admin/update-staff/:licenceNumber', component: StaffUpdateComponent },
      { path: 'admin/create-specialization', component: SpecializationCreateComponent },
      { path: 'admin/list-all-specialization', component: ListAllSpecializationComponent },
      { path: 'admin/list-operationtype', component: ListOperationtypeComponent},
      { path: 'admin/create-medical-condition', component: CreateMedicalConditionComponent },
      { path: 'admin/list-medical-condition', component: MedicalConditionListComponent },
      { path: 'admin/create-allergy', component: AllergyCreateComponent },
      { path: 'admin/update-allergy', component: AllergyUpdateComponent },
      { path: 'admin/list-allergy', component: AllergyListComponent },

      //Patient routes
      { path: 'patient/update-user', component: UpdateUserComponent },
      { path: 'patient/delete-user', component: DeleteUserComponent },

      //Doctor routes
      { path: 'doctor/create-operation', component: RegisterOperationComponent, canActivate: [AuthGuard], data: {role: 'Doctor'}  },
      { path: 'doctor/update-operation', component: UpdateOperationComponent, canActivate: [AuthGuard], data: {role: 'Doctor'}  },
      { path: 'doctor/list-operation', component: ListAllOperationComponent, canActivate: [AuthGuard], data: {role: 'Doctor'}  },
      { path: 'doctor/delete-operation', component: DeleteOperationComponent, canActivate: [AuthGuard], data: {role: 'Doctor'}  },
      { path: 'doctor/list-medical-condition', component: MedicalConditionListComponent},
      { path: 'doctor/list-allergy', component: AllergyListComponent },
      { path: 'doctor/medical-records', component: MedicalRecordDashComponent, canActivate: [AuthGuard], data: { role: 'Doctor' } },
      { path: 'doctor/hospital3D-viewer', component: ThreejsViewerComponent, canActivate: [AuthGuard], data: { role: 'Doctor' } },
      { path: 'doctor/appointments', component: AppointmentDashComponent, canActivate: [AuthGuard], data: { role: 'Doctor' } },

    ]



  },

  { path: 'admin', component: AdminMenuComponent, canActivate: [AuthGuard], data: {role : 'Admin'} },
  { path: 'patient', component: PatientMenuComponent , canActivate: [AuthGuard], data: { role: 'Patient' } }, //Mete tu se quiseres Rui
  { path: 'patient', component: PatientMenuComponent },
  { path: 'doctor', component: DoctorMenuComponent, canActivate: [AuthGuard], data: {role: 'Doctor'} },
  { path: 'unauthorized', component: UnauthorizedComponent },
  { path: '**', redirectTo: '/main' },
  { path: '', redirectTo: '/main', pathMatch: 'full' }

];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
