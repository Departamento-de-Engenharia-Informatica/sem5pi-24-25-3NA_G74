import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';
import { AdminMenuComponent } from './presentation/components/admin-menu/admin-menu.component';
import {RegisterUserComponent} from './presentation/components/register-user/register-user.component';
import {UpdateUserComponent} from './presentation/components/update-user/update-user.component';
import {DeleteUserComponent} from './presentation/components/delete-user/delete-user.component';
import {MainMenuComponent} from './presentation/components/main-menu/main-menu.component';
import {PatientMenuComponent} from './presentation/components/patient-menu/patient-menu.component';
import { DoctorMenuComponent } from './presentation/components/doctor-menu/doctor-menu.component';
import { RegisterOperationComponent } from './presentation/components/register-operation/register-operation.component';
import { UpdateOperationComponent } from './presentation/components/update-operation/update-operation.component';
import { ListAllOperationComponent } from './presentation/components/list-all-operation/list-all-operation.component';
import { DeleteOperationComponent } from './presentation/components/delete-operation/delete-operation.component';


const routes: Routes = [
  { path: '', redirectTo: '/main', pathMatch: 'full' },  // Redirect root to the main menu
  { path: 'main', component: MainMenuComponent },
  { path: 'admin', component: AdminMenuComponent },
  { path: 'admin/create-patient', component: PatientCreateComponent },
  { path: 'admin/update-patient', component: PatientUpdateComponent },
  { path: 'admin/delete-patient', component: PatientDeleteComponent },
  { path: 'admin/register-user', component: RegisterUserComponent},
  { path: 'patient', component: PatientMenuComponent},
  { path: 'register-user', component: RegisterUserComponent},
  { path: 'update-user', component: UpdateUserComponent},
  { path: 'delete-user', component: DeleteUserComponent},
  { path: 'doctor', component: DoctorMenuComponent},
  { path: 'doctor/create-operation', component:RegisterOperationComponent},
  { path: 'doctor/update-operation', component:UpdateOperationComponent},
  { path: 'doctor/list-operation', component:ListAllOperationComponent},
  { path: 'doctor/delete-operation', component:DeleteOperationComponent},

  
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
