import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule } from '@angular/common/http';
import { AppComponent } from './app.component';
import { AdminMenuComponent } from './presentation/components/admin-menu/admin-menu.component';
import { PatientCreateComponent } from './presentation/components/patient-create/patient-create.component';
import { PatientUpdateComponent } from './presentation/components/patient-update/patient-update.component';
import { PatientDeleteComponent } from './presentation/components/patient-delete/patient-delete.component';
import { AppRoutingModule } from './app-routing.module';
import { FormsModule } from '@angular/forms';  // Import FormsModule

@NgModule({
  declarations: [
    AppComponent,
    AdminMenuComponent,
    PatientCreateComponent,  // Add the component here
    PatientUpdateComponent,
    PatientDeleteComponent
  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    FormsModule,
    AppRoutingModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule {}