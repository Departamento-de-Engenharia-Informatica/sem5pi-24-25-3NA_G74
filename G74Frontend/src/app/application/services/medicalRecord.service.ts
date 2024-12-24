import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { Staff } from '../../domain/models/staff.model';
import { environment } from '../../../environments/environment';
import { HttpClient } from '@angular/common/http';
import { catchError, tap } from 'rxjs/operators';

@Injectable({
    providedIn: 'root'
})
export class StaffService {
    http = inject(HttpClient);

    private apiUrl = `${environment.apiUrlNode}/medical-record/`;

    createMedicalRecord(){
        //TODO:call node api
    }

    updateMedicalRecord(){
       //TODO:call node api
    }

    searchMedicalRecord(){
        //TODO:call node api
    }

    deleteMedicalRecord(){
        //TODO:call node api
    }


}
