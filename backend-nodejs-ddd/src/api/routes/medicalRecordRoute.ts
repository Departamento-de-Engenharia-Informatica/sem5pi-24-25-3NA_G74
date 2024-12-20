import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';
import { Container } from 'typedi';
import MedicalRecordService from '../../services/medicalRecordService';

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
      }),
    }),
    async (req, res, next) => {
      try {
        const medicalRecordServiceInstance = Container.get(MedicalRecordService);
        const record = await medicalRecordServiceInstance.updateByPatientId(req.params.patientId, req.body);
        return res.status(200).json({ record });
      } catch (e) {
        return next(e);
      }
    },
  );
};
