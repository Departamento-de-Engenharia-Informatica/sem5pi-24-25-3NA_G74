import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';
import { Container } from 'typedi';
import MedicalRecordService from '../../services/medicalRecordService';
import MedicalRecordController from '../../controllers/medicalRecordController';

const route = Router();

export default (app: Router) => {
  app.use('/medical-record', route);

  // Get all medical records
  route.get(
    '', // empty string means this handles the base route /medical-record
    async (req, res, next) => {
      try {
        const medicalRecordServiceInstance = Container.get(MedicalRecordService);
        const { records } = await medicalRecordServiceInstance.getAll();
        return res.status(200).json({ records });
      } catch (e) {
        return next(e);
      }
    },
  );

  route.get(
    '/:patientId',
    celebrate({
      params: Joi.object({
        patientId: Joi.string().required(),
      }),
    }),
    async (req, res, next) => {
      try {
        console.log(req.body)
        const medicalRecordServiceInstance = Container.get(MedicalRecordService);
        const record = await medicalRecordServiceInstance.getByPatientId(req.params.patientId);
        return res.status(200).json({ record });
      } catch (e) {
        return next(e);
      }
    },
  );

  route.post(
    '',
    celebrate({
      body: Joi.object({
        patientId: Joi.string().required(),
        freeText: Joi.string(),
      }),
    }),
    async (req, res, next) => {
      try {
        const medicalRecordServiceInstance = Container.get(MedicalRecordService);
        const record = await medicalRecordServiceInstance.create(req.body);
        return res.status(201).json({ record });
      } catch (e) {
        return next(e);
      }
    },
  );

  route.patch(
    '/:patientId',
    celebrate({
      params: Joi.object({
        patientId: Joi.string().required(),
      }),
      body: Joi.object({
        freeText: Joi.string().allow('', null),
        allergies: Joi.array().items(Joi.string().required()).optional(),
        medicalConditions: Joi.array().items(Joi.string().required()).optional(),
      }),
    }),
    async (req, res, next) => {
      try {
        
        const medicalRecordControllerInstance = Container.get(MedicalRecordController);
        const record = await medicalRecordControllerInstance.updateByPatientId(req.params.patientId, req.body);
        return res.status(200).json({ record });
      } catch (e) {
        return next(e);
      }
    },
  );

  route.get(
    '/medicalCondition/:medicalConditionCode',
    celebrate({
      params: Joi.object({
        medicalConditionCode: Joi.string().required(),
      })
    }),
    async (req, res, next) =>{
      try {
        console.log('Received request for medical condition:', req.params.medicalConditionCode);
        const medicalRecordControllerInstance = Container.get(MedicalRecordController);
        const records = await medicalRecordControllerInstance.findByMedicalCondition(req.params.medicalConditionCode);
        console.log('Sending response with records');
        return res.status(200).json({ records });
      } catch (e) {
        console.error('Error in route handler:', e);
        return next(e);
      }
    }
  )

  route.get(
    '/allergy/:allergyCode',
    celebrate({
      params: Joi.object({
        allergyCode: Joi.string().required(),
      })
    }),
    async (req, res, next) =>{
      try {
        console.log('Received request for allergy:', req.params.allergyCode);
        const medicalRecordControllerInstance = Container.get(MedicalRecordController);
        const records = await medicalRecordControllerInstance.findByAllergy(req.params.allergyCode);
        console.log('Sending response with records');
        return res.status(200).json({ records });
      } catch (e) {
        console.error('Error in route handler:', e);
        return next(e);
      }
    }
  )

  
};
