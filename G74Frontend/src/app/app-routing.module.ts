import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';

//Para os futuros a ver isso, não existe uma home ainda por isso o default por agora é redirecionar para o create patient (path: '') 

const routes: Routes = [
  { path: '', redirectTo: '/create-patient', pathMatch: 'full' },  // Redirect root path to 'create-patient' for testing
  { path: 'create-patient', component: PatientCreateComponent },
  { path: 'update-patient', component: PatientUpdateComponent },
  { path: 'delete-patient', component: PatientDeleteComponent}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
