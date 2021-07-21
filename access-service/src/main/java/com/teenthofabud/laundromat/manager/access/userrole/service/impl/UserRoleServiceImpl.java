package com.teenthofabud.laundromat.manager.access.userrole.service.impl;

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
import com.teenthofabud.laundromat.manager.access.userrole.converter.UserRoleDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.userrole.converter.UserRoleEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.userrole.converter.UserRoleForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.userrole.data.*;
import com.teenthofabud.laundromat.manager.access.userrole.mapper.UserRoleEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.userrole.mapper.UserRoleForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.userrole.repository.UserRoleRepository;
import com.teenthofabud.laundromat.manager.access.userrole.service.UserRoleService;
import com.teenthofabud.laundromat.manager.access.userrole.validator.UserRoleDtoValidator;
import com.teenthofabud.laundromat.manager.access.userrole.validator.UserRoleFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.userrole.validator.UserRoleFormValidator;
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
public class UserRoleServiceImpl implements UserRoleService {

    private static final Comparator<UserRoleVo> CMP_BY_ID = (s1, s2) -> {
        return Long.compare(s1.getId(), s2.getId());
    };

    private UserRoleEntity2VoConverter entity2VoConverter;
    private UserRoleForm2EntityConverter form2EntityConverter;
    private UserRoleDto2EntityConverter dto2EntityConverter;
    private UserRoleForm2EntityMapper form2EntityMapper;
    private UserRoleEntitySelfMapper entitySelfMapper;
    private UserRoleFormValidator formValidator;
    private UserRoleFormRelaxedValidator relaxedFormValidator;
    private UserRoleDtoValidator dtoValidator;
    private UserRoleRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(UserRoleEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(UserRoleDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(UserRoleForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(UserRoleEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(UserRoleFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchUserRoleValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(UserRoleDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(UserRoleForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(UserRoleRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(UserRoleFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<UserRoleVo> entity2DetailedVoList(List<UserRoleEntity> userRoleEntityList) {
        List<UserRoleVo> userRoleDetailsList = new ArrayList<>(userRoleEntityList.size());
        for(UserRoleEntity entity : userRoleEntityList) {
            UserRoleVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            userRoleDetailsList.add(vo);
        }
        return userRoleDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<UserRoleVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all UserRoleEntity by their natural ordering");
        List<UserRoleEntity> userRoleEntityList = repository.findAll();
        Set<UserRoleVo> naturallyOrderedSet = new TreeSet<UserRoleVo>(CMP_BY_ID);
        for(UserRoleEntity entity : userRoleEntityList) {
            UserRoleVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} UserRoleVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public UserRoleVo retrieveDetailsById(Long id) throws UserRoleException {
        log.info("Requesting UserRoleEntity by id: {}", id);
        Optional<UserRoleEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No UserRoleEntity found by id: {}", id);
            throw new UserRoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        UserRoleEntity entity = optEntity.get();
        UserRoleVo vo = entity2VoConverter.convert(entity);
        log.info("Found UserRoleVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<UserRoleVo> retrieveAllMatchingDetailsByUserType(Long userTypeId) throws UserRoleException {
        log.info("Requesting UserRoleEntity that match with userTypeId: {}", userTypeId);
        List<UserRoleEntity> userRoleEntityList = repository.findByUserTypeId(userTypeId);
        if(userRoleEntityList != null && !userRoleEntityList.isEmpty()) {
            List<UserRoleVo> matchedUserRoleList = entity2DetailedVoList(userRoleEntityList);
            log.info("Found {} UserRoleVo matching with userTypeId: {}", matchedUserRoleList.size(),userTypeId);
            return matchedUserRoleList;
        }
        log.debug("No UserRoleVo found matching with userTypeId: {}", userTypeId);
        throw new UserRoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "userTypeId", userTypeId });
    }

    @Override
    @Transactional(readOnly = true)
    public List<UserRoleVo> retrieveAllMatchingDetailsByRole(Long roleId) throws UserRoleException {
        log.info("Requesting UserRoleEntity that match with roleId: {}", roleId);
        List<UserRoleEntity> userRoleEntityList = repository.findByRoleId(roleId);
        if(userRoleEntityList != null && !userRoleEntityList.isEmpty()) {
            List<UserRoleVo> matchedUserRoleList = entity2DetailedVoList(userRoleEntityList);
            log.info("Found {} UserRoleVo matching with roleId: {}", matchedUserRoleList.size(),roleId);
            return matchedUserRoleList;
        }
        log.debug("No UserRoleVo found matching with roleId: {}", roleId);
        throw new UserRoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "roleId", roleId });
    }

    @Override
    @Transactional
    public Long createUserRole(UserRoleForm form) throws UserRoleException {
        log.info("Creating new UserRoleEntity");

        if(form == null) {
            log.debug("UserRoleForm provided is null");
            throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of UserRoleForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("UserRoleForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("UserRoleForm error detail: {}", ec);
            throw new UserRoleException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of UserRoleForm are valid");

        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                form.getUserTypeId(), form.getRoleId());
        if(repository.existsByUserTypeIdAndRoleId(form.getUserTypeId(), form.getRoleId())) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    form.getUserTypeId(), form.getRoleId());
            throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "userTypeId: " + form.getUserTypeId(), "roleId: " + form.getRoleId() });
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                form.getUserTypeId(), form.getRoleId());

        UserRoleEntity expectedEntity = form2EntityConverter.convert(form);
        log.debug("Saving {}", expectedEntity);
        UserRoleEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new UserRoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist UserRoleForm details" });
        }
        log.info("Created new UserRoleForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateUserRole(Long id, UserRoleForm form) throws UserRoleException {
        log.info("Updating UserRoleForm by id: {}", id);

        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_USER_ROLE_ENTITY_ID.getValue(), id);
        Optional<UserRoleEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_NO_USER_ROLE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new UserRoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_FOUND_USER_ROLE_ENTITY_ID.getValue(), id);

        UserRoleEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("UserRoleEntity is inactive with id: {}", id);
            throw new UserRoleException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("UserRoleEntity is active with id: {}", id);

        if(form == null) {
            log.debug("UserRoleForm is null");
            throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of UserRoleForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("UserRoleForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("UserRoleForm error detail: {}", ec);
            throw new UserRoleException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of UserRoleForm are empty");
            throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of UserRoleForm are valid");

        UserRoleEntity expectedEntity = null;

        try {
            Optional<UserRoleEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
            if(optExpectedEntity.isEmpty()) {
                log.debug("No new value for attributes of UserRoleForm");
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
            }
            log.debug("Successfully compared and copied attributes from UserRoleForm to UserRoleEntity");
            expectedEntity = optExpectedEntity.get();
        } catch (TOABBaseException e) {
            throw (UserRoleException) e;
        }

        checkUniquenessOfUserRoleModel(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from UserRoleEntity to UserRoleForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new UserRoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist  userRole details" });
        }
        log.info("Updated existing UserRoleEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteUserRole(Long id) throws UserRoleException {
        log.info("Soft deleting UserRoleEntity by id: {}", id);

        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_USER_ROLE_ENTITY_ID.getValue(), id);
        Optional<UserRoleEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_NO_USER_ROLE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new UserRoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_FOUND_USER_ROLE_ENTITY_ID.getValue(), id);

        UserRoleEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("UserRoleEntity is inactive with id: {}", id);
            throw new UserRoleException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("UserRoleEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        UserRoleEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new UserRoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current userRole details with id:" + id });
        }

        log.info("Soft deleted existing UserRoleEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnUserRole(Long id, List<PatchOperationForm> patches) throws UserRoleException {
        log.info("Patching UserRoleEntity by id: {}", id);

        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_USER_ROLE_ENTITY_ID.getValue(), id);
        Optional<UserRoleEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_NO_USER_ROLE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new UserRoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_FOUND_USER_ROLE_ENTITY_ID.getValue(), id);

        UserRoleEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("UserRole patch list not provided");
            throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("UserRole patch list has {} items", patches.size());


        log.debug("Validating patch list items for UserRole");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All UserRole patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the UserRole patch item are invalid");
            throw new UserRoleException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for UserRole");


        log.debug("Patching list items to UserRoleDto");
        UserRoleDto patchedUserRoleForm = new UserRoleDto();
        try {
            log.debug("Preparing patch list items for UserRole");
            JsonNode userRoleDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch userRolePatch = JsonPatch.fromJson(userRoleDtoTree);
            log.debug("Prepared patch list items for UserRole");
            JsonNode blankUserRoleDtoTree = om.convertValue(new UserRoleDto(), JsonNode.class);
            JsonNode patchedUserRoleFormTree = userRolePatch.apply(blankUserRoleDtoTree);
            log.debug("Applying patch list items to UserRoleDto");
            patchedUserRoleForm = om.treeToValue(patchedUserRoleFormTree, UserRoleDto.class);
            log.debug("Applied patch list items to UserRoleDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to UserRoleDto: {}", e);
            UserRoleException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in UserRoleDto");
                ex = new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new UserRoleException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to UserRoleDto: {}", e);
            throw new UserRoleException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to UserRoleDto");

        log.debug("Validating patched UserRoleDto");
        Errors err = new DirectFieldBindingResult(patchedUserRoleForm, patchedUserRoleForm.getClass().getSimpleName());
        dtoValidator.validate(patchedUserRoleForm, err);
        if(err.hasErrors()) {
            log.debug("Patched UserRoleDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched UserRoleDto error detail: {}", ec);
            throw new UserRoleException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched UserRoleDto are valid");

        checkUniquenessOfUserRoleModel(patchedUserRoleForm, actualEntity);

        log.debug("Comparatively copying patched attributes from UserRoleDto to UserRoleEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedUserRoleForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (UserRoleException) e;
        }
        log.debug("Comparatively copied patched attributes from UserRoleDto to UserRoleEntity");

        log.debug("Saving patched UserRoleEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched UserRoleEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete UserRoleEntity with id:{}", id);
            throw new UserRoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch  userRole details with id:" + id });
        }
        log.info("Patched UserRoleEntity with id:{}", id);
    }

    private void checkUniquenessOfUserRoleModel(UserRoleDto patchedUserRoleForm, UserRoleEntity actualEntity) throws UserRoleException {
        // userTypeId = true, roleId = false
        if(patchedUserRoleForm.getUserTypeId().isPresent() && patchedUserRoleForm.getRoleId().isEmpty()) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    patchedUserRoleForm.getUserTypeId().get(), actualEntity.getRole().getId());
            Long userTypeId = Long.parseLong(patchedUserRoleForm.getUserTypeId().get());
            boolean duplicateEntitySw =  repository.existsByUserTypeIdAndRoleId(userTypeId, actualEntity.getRole().getId());
            if(duplicateEntitySw) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                        patchedUserRoleForm.getUserTypeId().get(), actualEntity.getRole().getId());
                throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "userTypeId " + patchedUserRoleForm.getUserTypeId().get(), "roleId " + actualEntity.getRole().getId() });
            }
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    patchedUserRoleForm.getUserTypeId().get(), actualEntity.getRole().getId());
        }

        // userTypeId = false, roleId = true
        if(patchedUserRoleForm.getUserTypeId().isEmpty() && patchedUserRoleForm.getRoleId().isPresent()) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    actualEntity.getUserType().getId(), patchedUserRoleForm.getRoleId().get());
            Long roleId = Long.parseLong(patchedUserRoleForm.getRoleId().get());
            boolean duplicateEntitySw =  repository.existsByUserTypeIdAndRoleId(actualEntity.getUserType().getId(), roleId);
            if(duplicateEntitySw) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                        actualEntity.getUserType().getId(), patchedUserRoleForm.getRoleId().get());
                throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "userTypeId " + actualEntity.getUserType().getId(), "roleId " + patchedUserRoleForm.getRoleId().get() });
            }
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    actualEntity.getUserType().getId(), patchedUserRoleForm.getRoleId().get());
        }

        // userTypeId = true, roleId = true
        if(patchedUserRoleForm.getUserTypeId().isEmpty() && patchedUserRoleForm.getRoleId().isPresent()) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    patchedUserRoleForm.getUserTypeId().get(), patchedUserRoleForm.getRoleId().get());
            Long userTypeId = Long.parseLong(patchedUserRoleForm.getUserTypeId().get());
            Long roleId = Long.parseLong(patchedUserRoleForm.getRoleId().get());
            boolean duplicateEntitySw =  repository.existsByUserTypeIdAndRoleId(userTypeId, roleId);
            if(duplicateEntitySw) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                        patchedUserRoleForm.getUserTypeId().get(), patchedUserRoleForm.getRoleId().get());
                throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "userTypeId " + patchedUserRoleForm.getUserTypeId().get(), "roleId " + patchedUserRoleForm.getRoleId().get() });
            }
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    patchedUserRoleForm.getUserTypeId().get(), patchedUserRoleForm.getRoleId().get());
        }
    }

    private void checkUniquenessOfUserRoleModel(UserRoleForm userRoleForm, UserRoleEntity actualEntity) throws UserRoleException {
        // userTypeId = true, roleId = false
        if(userRoleForm.getUserTypeId() != null && userRoleForm.getRoleId() == null) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    userRoleForm.getUserTypeId(), actualEntity.getRole().getId());
            boolean duplicateEntitySw =  repository.existsByUserTypeIdAndRoleId(userRoleForm.getUserTypeId(), actualEntity.getRole().getId());
            if(duplicateEntitySw) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                        userRoleForm.getUserTypeId(), actualEntity.getRole().getId());
                throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "userTypeId " + userRoleForm.getUserTypeId(), "roleId " + actualEntity.getRole().getId() });
            }
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    userRoleForm.getUserTypeId(), actualEntity.getRole().getId());
        }

        // userTypeId = false, roleId = true
        if(userRoleForm.getUserTypeId() == null && userRoleForm.getRoleId() != null) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    actualEntity.getUserType().getId(), userRoleForm.getRoleId());
            boolean duplicateEntitySw =  repository.existsByUserTypeIdAndRoleId(actualEntity.getUserType().getId(), userRoleForm.getRoleId());
            if(duplicateEntitySw) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                        actualEntity.getUserType().getId(), userRoleForm.getRoleId());
                throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "userTypeId " + actualEntity.getUserType().getId(), "roleId " + userRoleForm.getRoleId() });
            }
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    actualEntity.getUserType().getId(), userRoleForm.getRoleId());
        }

        // userTypeId = true, roleId = true
        if(userRoleForm.getUserTypeId() != null && userRoleForm.getRoleId() != null) {
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    userRoleForm.getUserTypeId(), userRoleForm.getRoleId());
            boolean duplicateEntitySw =  repository.existsByUserTypeIdAndRoleId(userRoleForm.getUserTypeId(), userRoleForm.getRoleId());
            if(duplicateEntitySw) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                        userRoleForm.getUserTypeId(), userRoleForm.getRoleId());
                throw new UserRoleException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "userTypeId " + userRoleForm.getUserTypeId(), "roleId " + userRoleForm.getRoleId() });
            }
            log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID.getValue(),
                    userRoleForm.getUserTypeId(), userRoleForm.getRoleId());
        }
    }

}