import { HttpClient } from "@angular/common/http";
import { inject, Injectable } from "@angular/core";
import { environment } from "../../../environments/environment";
import { AppointmentDTO } from "../../dto/appointment.dto";
import { catchError, of } from "rxjs";

@Injectable({
    providedIn: 'root'
})
export class AppointmentService
{
    http = inject(HttpClient);

    private apiUrl = `${environment.apiUrl}/Appointment`;


    create(appointmentDTO: AppointmentDTO){
        return this.http.post<AppointmentDTO>(this.apiUrl, appointmentDTO)
                  .pipe(
                    catchError(error => {
                      console.error('Error creating appointment:', error);
                      return of(null);
                    })
                  );
    }
}