package com.teenthofabud.laundromat.manager.access.usertype.service.impl;

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
import com.teenthofabud.laundromat.manager.access.usertype.converter.UserTypeDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.usertype.converter.UserTypeEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.usertype.converter.UserTypeForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.usertype.data.*;
import com.teenthofabud.laundromat.manager.access.usertype.mapper.UserTypeEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.usertype.mapper.UserTypeForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.usertype.repository.UserTypeRepository;
import com.teenthofabud.laundromat.manager.access.usertype.service.UserTypeService;
import com.teenthofabud.laundromat.manager.access.usertype.validator.UserTypeDtoValidator;
import com.teenthofabud.laundromat.manager.access.usertype.validator.UserTypeFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.usertype.validator.UserTypeFormValidator;
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
public class UserTypeServiceImpl implements UserTypeService {

    private static final Comparator<UserTypeVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private UserTypeEntity2VoConverter entity2VoConverter;
    private UserTypeForm2EntityConverter form2EntityConverter;
    private UserTypeDto2EntityConverter dto2EntityConverter;
    private UserTypeForm2EntityMapper form2EntityMapper;
    private UserTypeEntitySelfMapper entitySelfMapper;
    private UserTypeFormValidator formValidator;
    private UserTypeFormRelaxedValidator relaxedFormValidator;
    private UserTypeDtoValidator dtoValidator;
    private UserTypeRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(UserTypeEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(UserTypeDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(UserTypeForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(UserTypeEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(UserTypeFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchUserTypeValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(UserTypeDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(UserTypeForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(UserTypeRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(UserTypeFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<UserTypeVo> entity2DetailedVoList(List<UserTypeEntity> userTypeEntityList) {
        List<UserTypeVo> userTypeDetailsList = new ArrayList<>(userTypeEntityList.size());
        for(UserTypeEntity entity : userTypeEntityList) {
            UserTypeVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            userTypeDetailsList.add(vo);
        }
        return userTypeDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<UserTypeVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all UserTypeEntity by their natural ordering");
        List<UserTypeEntity> userTypeEntityList = repository.findAll();
        Set<UserTypeVo> naturallyOrderedSet = new TreeSet<UserTypeVo>(CMP_BY_NAME);
        for(UserTypeEntity entity : userTypeEntityList) {
            UserTypeVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} UserTypeVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public UserTypeVo retrieveDetailsById(long id) throws UserTypeException {
        log.info("Requesting UserTypeEntity by id: {}", id);
        Optional<UserTypeEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No UserTypeEntity found by id: {}", id);
            throw new UserTypeException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        UserTypeEntity entity = optEntity.get();
        UserTypeVo vo = entity2VoConverter.convert(entity);
        log.info("Found UserTypeVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<UserTypeVo> retrieveAllMatchingDetailsByName(String name) throws UserTypeException {
        log.info("Requesting UserTypeEntity that match with name: {}", name);
        List<UserTypeEntity> userTypeEntityList = repository.findByNameContaining(name);
        if(userTypeEntityList != null && !userTypeEntityList.isEmpty()) {
            List<UserTypeVo> matchedUserTypeList = entity2DetailedVoList(userTypeEntityList);
            log.info("Found {} UserTypeVo matching with name: {}", matchedUserTypeList.size(),name);
            return matchedUserTypeList;
        }
        log.debug("No UserTypeVo found matching with name: {}", name);
        throw new UserTypeException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createUserType(UserTypeForm form) throws UserTypeException {
        log.info("Creating new UserTypeEntity");

        if(form == null) {
            log.debug("UserTypeForm provided is null");
            throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of UserTypeForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("UserTypeForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("UserTypeForm error detail: {}", ec);
            throw new UserTypeException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of UserTypeForm are valid");

        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_EXISTENCE_BY_NAME.getValue(), form.getName());
        UserTypeEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new UserTypeException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        UserTypeEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new UserTypeException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist UserTypeForm details" });
        }
        log.info("Created new UserTypeForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateUserType(Long id, UserTypeForm form) throws UserTypeException {
        log.info("Updating UserTypeForm by id: {}", id);

        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_USER_TYPE_ENTITY_ID.getValue(), id);
        Optional<UserTypeEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_NO_USER_TYPE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new UserTypeException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_FOUND_USER_TYPE_ENTITY_ID.getValue(), id);

        UserTypeEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("UserTypeEntity is inactive with id: {}", id);
            throw new UserTypeException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("UserTypeEntity is active with id: {}", id);

        if(form == null) {
            log.debug("UserTypeForm is null");
            throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of UserTypeForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("UserTypeForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("UserTypeForm error detail: {}", ec);
            throw new UserTypeException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of UserTypeForm are empty");
            throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of UserTypeForm are valid");

        Optional<UserTypeEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of UserTypeForm");
            throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from UserTypeForm to UserTypeEntity");

        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_EXISTENCE_BY_NAME.getValue(), form.getName());
        UserTypeEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new UserTypeException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from UserTypeEntity to UserTypeForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new UserTypeException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency userType details" });
        }
        log.info("Updated existing UserTypeEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteUserType(Long id) throws UserTypeException {
        log.info("Soft deleting UserTypeEntity by id: {}", id);

        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_USER_TYPE_ENTITY_ID.getValue(), id);
        Optional<UserTypeEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_NO_USER_TYPE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new UserTypeException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_FOUND_USER_TYPE_ENTITY_ID.getValue(), id);

        UserTypeEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("UserTypeEntity is inactive with id: {}", id);
            throw new UserTypeException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("UserTypeEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        UserTypeEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new UserTypeException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current userType details with id:" + id });
        }

        log.info("Soft deleted existing UserTypeEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnUserType(Long id, List<PatchOperationForm> patches) throws UserTypeException {
        log.info("Patching UserTypeEntity by id: {}", id);

        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_USER_TYPE_ENTITY_ID.getValue(), id);
        Optional<UserTypeEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_NO_USER_TYPE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new UserTypeException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_FOUND_USER_TYPE_ENTITY_ID.getValue(), id);

        UserTypeEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("UserType patch list not provided");
            throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("UserType patch list has {} items", patches.size());


        log.debug("Validating patch list items for UserType");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All UserType patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the UserType patch item are invalid");
            throw new UserTypeException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for UserType");


        log.debug("Patching list items to UserTypeDto");
        UserTypeDto patchedUserTypeForm = new UserTypeDto();
        try {
            log.debug("Preparing patch list items for UserType");
            JsonNode userTypeDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch userTypePatch = JsonPatch.fromJson(userTypeDtoTree);
            log.debug("Prepared patch list items for UserType");
            JsonNode blankUserTypeDtoTree = om.convertValue(new UserTypeDto(), JsonNode.class);
            JsonNode patchedUserTypeFormTree = userTypePatch.apply(blankUserTypeDtoTree);
            log.debug("Applying patch list items to UserTypeDto");
            patchedUserTypeForm = om.treeToValue(patchedUserTypeFormTree, UserTypeDto.class);
            log.debug("Applied patch list items to UserTypeDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to UserTypeDto: {}", e);
            UserTypeException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in UserTypeDto");
                ex = new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new UserTypeException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to UserTypeDto: {}", e);
            throw new UserTypeException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to UserTypeDto");

        log.debug("Validating patched UserTypeDto");
        Errors err = new DirectFieldBindingResult(patchedUserTypeForm, patchedUserTypeForm.getClass().getSimpleName());
        dtoValidator.validate(patchedUserTypeForm, err);
        if(err.hasErrors()) {
            log.debug("Patched UserTypeDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched UserTypeDto error detail: {}", ec);
            throw new UserTypeException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched UserTypeDto are valid");

        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_EXISTENCE_BY_NAME.getValue(), patchedUserTypeForm.getName().get());
        if(actualEntity.getName().compareTo(patchedUserTypeForm.getName().get()) == 0 || repository.existsByName(patchedUserTypeForm.getName().get())) {
            log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_EXISTS_BY_NAME.getValue(), patchedUserTypeForm.getName().get());
            throw new UserTypeException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", patchedUserTypeForm.getName().get() });
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_NON_EXISTENCE_BY_NAME.getValue(), patchedUserTypeForm.getName().get());


        log.debug("Comparatively copying patched attributes from UserTypeDto to UserTypeEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedUserTypeForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (UserTypeException) e;
        }
        log.debug("Comparatively copied patched attributes from UserTypeDto to UserTypeEntity");

        log.debug("Saving patched UserTypeEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched UserTypeEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete UserTypeEntity with id:{}", id);
            throw new UserTypeException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency userType details with id:" + id });
        }
        log.info("Patched UserTypeEntity with id:{}", id);
    }
}