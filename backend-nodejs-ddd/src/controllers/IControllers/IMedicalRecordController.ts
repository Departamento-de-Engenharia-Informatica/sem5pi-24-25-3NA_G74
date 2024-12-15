import { NextFunction, Request, Response } from "express";


export default interface IMedicalRecordController{
    updateMedicalCondition(req: Request, res: Response, next: NextFunction);
}