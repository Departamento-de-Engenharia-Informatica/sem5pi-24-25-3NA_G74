import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';

const routes: Routes = [
  { path: 'create-patient', component: PatientCreateComponent },
  { path: '', redirectTo: '/create-patient', pathMatch: 'full' }  // Redirect root path to 'create-patient' for testing
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule {}
