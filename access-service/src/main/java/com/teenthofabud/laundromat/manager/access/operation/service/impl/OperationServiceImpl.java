package com.teenthofabud.laundromat.manager.access.operation.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.converter.OperationDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.operation.converter.OperationEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.operation.converter.OperationForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.operation.data.*;
import com.teenthofabud.laundromat.manager.access.operation.mapper.OperationEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.operation.mapper.OperationForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.operation.repository.OperationRepository;
import com.teenthofabud.laundromat.manager.access.operation.service.OperationService;
import com.teenthofabud.laundromat.manager.access.operation.validator.OperationDtoValidator;
import com.teenthofabud.laundromat.manager.access.operation.validator.OperationFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.operation.validator.OperationFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class OperationServiceImpl implements OperationService {

    private static final Comparator<OperationVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private OperationEntity2VoConverter entity2VoConverter;
    private OperationForm2EntityConverter form2EntityConverter;
    private OperationDto2EntityConverter dto2EntityConverter;
    private OperationForm2EntityMapper form2EntityMapper;
    private OperationEntitySelfMapper entitySelfMapper;
    private OperationFormValidator formValidator;
    private OperationFormRelaxedValidator relaxedFormValidator;
    private OperationDtoValidator dtoValidator;
    private OperationRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(OperationEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(OperationDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(OperationForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(OperationEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(OperationFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchOperationValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(OperationDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(OperationForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(OperationRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(OperationFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<OperationVo> entity2DetailedVoList(List<OperationEntity> operationEntityList) {
        List<OperationVo> operationDetailsList = new ArrayList<>(operationEntityList.size());
        for(OperationEntity entity : operationEntityList) {
            OperationVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            operationDetailsList.add(vo);
        }
        return operationDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<OperationVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all OperationEntity by their natural ordering");
        List<OperationEntity> operationEntityList = repository.findAll();
        Set<OperationVo> naturallyOrderedSet = new TreeSet<OperationVo>(CMP_BY_NAME);
        for(OperationEntity entity : operationEntityList) {
            OperationVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} OperationVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public OperationVo retrieveDetailsById(long id) throws OperationException {
        log.info("Requesting OperationEntity by id: {}", id);
        Optional<OperationEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No OperationEntity found by id: {}", id);
            throw new OperationException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        OperationEntity entity = optEntity.get();
        OperationVo vo = entity2VoConverter.convert(entity);
        log.info("Found OperationVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<OperationVo> retrieveAllMatchingDetailsByName(String name) throws OperationException {
        log.info("Requesting OperationEntity that match with name: {}", name);
        List<OperationEntity> operationEntityList = repository.findByNameContaining(name);
        if(operationEntityList != null && !operationEntityList.isEmpty()) {
            List<OperationVo> matchedOperationList = entity2DetailedVoList(operationEntityList);
            log.info("Found {} OperationVo matching with name: {}", matchedOperationList.size(),name);
            return matchedOperationList;
        }
        log.debug("No OperationVo found matching with name: {}", name);
        throw new OperationException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createOperation(OperationForm form) throws OperationException {
        log.info("Creating new OperationEntity");

        if(form == null) {
            log.debug("OperationForm provided is null");
            throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of OperationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("OperationForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("OperationForm error detail: {}", ec);
            throw new OperationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of OperationForm are valid");

        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_EXISTENCE_BY_NAME.getValue(), form.getName());
        OperationEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new OperationException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        OperationEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new OperationException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist OperationForm details" });
        }
        log.info("Created new OperationForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateOperation(Long id, OperationForm form) throws OperationException {
        log.info("Updating OperationForm by id: {}", id);

        log.debug(OperationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_OPERATION_ENTITY_ID.getValue(), id);
        Optional<OperationEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(OperationMessageTemplate.MSG_TEMPLATE_NO_OPERATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new OperationException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_FOUND_OPERATION_ENTITY_ID.getValue(), id);

        OperationEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("OperationEntity is inactive with id: {}", id);
            throw new OperationException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("OperationEntity is active with id: {}", id);

        if(form == null) {
            log.debug("OperationForm is null");
            throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of OperationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("OperationForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("OperationForm error detail: {}", ec);
            throw new OperationException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of OperationForm are empty");
            throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of OperationForm are valid");

        Optional<OperationEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of OperationForm");
            throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from OperationForm to OperationEntity");

        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_EXISTENCE_BY_NAME.getValue(), form.getName());
        OperationEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new OperationException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from OperationEntity to OperationForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new OperationException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency operation details" });
        }
        log.info("Updated existing OperationEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteOperation(Long id) throws OperationException {
        log.info("Soft deleting OperationEntity by id: {}", id);

        log.debug(OperationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_OPERATION_ENTITY_ID.getValue(), id);
        Optional<OperationEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(OperationMessageTemplate.MSG_TEMPLATE_NO_OPERATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new OperationException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_FOUND_OPERATION_ENTITY_ID.getValue(), id);

        OperationEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("OperationEntity is inactive with id: {}", id);
            throw new OperationException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("OperationEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        OperationEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new OperationException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current operation details with id:" + id });
        }

        log.info("Soft deleted existing OperationEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnOperation(Long id, List<PatchOperationForm> patches) throws OperationException {
        log.info("Patching OperationEntity by id: {}", id);

        log.debug(OperationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_OPERATION_ENTITY_ID.getValue(), id);
        Optional<OperationEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(OperationMessageTemplate.MSG_TEMPLATE_NO_OPERATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new OperationException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_FOUND_OPERATION_ENTITY_ID.getValue(), id);

        OperationEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Operation patch list not provided");
            throw new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Operation patch list has {} items", patches.size());


        log.debug("Validating patch list items for Operation");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All Operation patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Operation patch item are invalid");
            throw new OperationException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Operation");


        log.debug("Patching list items to OperationDto");
        OperationDto patchedOperationForm = new OperationDto();
        try {
            log.debug("Preparing patch list items for Operation");
            JsonNode operationDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch operationPatch = JsonPatch.fromJson(operationDtoTree);
            log.debug("Prepared patch list items for Operation");
            JsonNode blankOperationDtoTree = om.convertValue(new OperationDto(), JsonNode.class);
            JsonNode patchedOperationFormTree = operationPatch.apply(blankOperationDtoTree);
            log.debug("Applying patch list items to OperationDto");
            patchedOperationForm = om.treeToValue(patchedOperationFormTree, OperationDto.class);
            log.debug("Applied patch list items to OperationDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to OperationDto: {}", e);
            OperationException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in OperationDto");
                ex = new OperationException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new OperationException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to OperationDto: {}", e);
            throw new OperationException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to OperationDto");

        log.debug("Validating patched OperationDto");
        Errors err = new DirectFieldBindingResult(patchedOperationForm, patchedOperationForm.getClass().getSimpleName());
        dtoValidator.validate(patchedOperationForm, err);
        if(err.hasErrors()) {
            log.debug("Patched OperationDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched OperationDto error detail: {}", ec);
            throw new OperationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched OperationDto are valid");

        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_EXISTENCE_BY_NAME.getValue(), patchedOperationForm.getName().get());
        if(actualEntity.getName().compareTo(patchedOperationForm.getName().get()) == 0 || repository.existsByName(patchedOperationForm.getName().get())) {
            log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_EXISTS_BY_NAME.getValue(), patchedOperationForm.getName().get());
            throw new OperationException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", patchedOperationForm.getName().get() });
        }
        log.debug(OperationMessageTemplate.MSG_TEMPLATE_OPERATION_NON_EXISTENCE_BY_NAME.getValue(), patchedOperationForm.getName().get());


        log.debug("Comparatively copying patched attributes from OperationDto to OperationEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedOperationForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (OperationException) e;
        }
        log.debug("Comparatively copied patched attributes from OperationDto to OperationEntity");

        log.debug("Saving patched OperationEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched OperationEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete OperationEntity with id:{}", id);
            throw new OperationException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency operation details with id:" + id });
        }
        log.info("Patched OperationEntity with id:{}", id);
    }
}