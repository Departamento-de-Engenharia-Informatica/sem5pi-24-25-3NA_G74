import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';
import { AdminMenuComponent } from './presentation/components/admin-menu/admin-menu.component';
import {RegisterUserComponent} from './presentation/components/register-user/register-user.component';
import {UpdateUserComponent} from './presentation/components/update-user/update-user.component';
import {DeleteUserComponent} from './presentation/components/delete-user/delete-user.component';


const routes: Routes = [
  { path: '', redirectTo: '/admin', pathMatch: 'full' },  // Redirect root to the admin menu
  { path: 'admin', component: AdminMenuComponent },       // Admin menu standalone route
  { path: 'admin/create-patient', component: PatientCreateComponent },
  { path: 'admin/update-patient', component: PatientUpdateComponent },
  { path: 'admin/delete-patient', component: PatientDeleteComponent },
  { path: 'register-user', component: RegisterUserComponent},
  { path: 'update-user', component: UpdateUserComponent},
  { path: 'delete-user', component: DeleteUserComponent}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
