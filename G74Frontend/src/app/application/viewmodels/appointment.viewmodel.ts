import { Observable, tap } from "rxjs";
import { AppointmentService } from "../../domain/services/appointment.service";
import { AppointmentDTO } from "../../dto/appointment.dto";
import { Injectable } from "@angular/core";

@Injectable({
    providedIn: 'root'
  })
export class AppointmentViewModel
{
    constructor (private appointmentService: AppointmentService){}

    create(appointmentDTO: AppointmentDTO): Observable<AppointmentDTO> {
        return this.appointmentService.create(appointmentDTO).pipe(
            tap({
                next: (result: AppointmentDTO | null) => {
                    if (result) {
                        // handle the result
                        console.log('Appointment created:', result);
                    } else {
                        console.error('Failed to create appointment');
                    }
                },
                error: (err) => {
                    console.error('Error creating appointment:', err);
                }
            })
        ) as Observable<AppointmentDTO>;
    }
}