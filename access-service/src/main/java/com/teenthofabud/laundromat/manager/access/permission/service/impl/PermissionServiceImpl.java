package com.teenthofabud.laundromat.manager.access.permission.service.impl;

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
import com.teenthofabud.laundromat.manager.access.permission.converter.PermissionDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.permission.converter.PermissionEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.permission.converter.PermissionForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionDto;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import com.teenthofabud.laundromat.manager.access.permission.mapper.PermissionEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.permission.mapper.PermissionForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.permission.repository.PermissionRepository;
import com.teenthofabud.laundromat.manager.access.permission.service.PermissionService;
import com.teenthofabud.laundromat.manager.access.permission.validator.PermissionDtoValidator;
import com.teenthofabud.laundromat.manager.access.permission.validator.PermissionFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.permission.validator.PermissionFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Component
@Slf4j
public class PermissionServiceImpl implements PermissionService {

    private static final Comparator<PermissionVo> CMP_BY_ID = (s1, s2) -> {
        return Long.compare(s1.getId(), s2.getId());
    };

    private PermissionEntity2VoConverter entity2VoConverter;
    private PermissionForm2EntityConverter form2EntityConverter;
    private PermissionDto2EntityConverter dto2EntityConverter;
    private PermissionForm2EntityMapper form2EntityMapper;
    private PermissionEntitySelfMapper entitySelfMapper;
    private PermissionFormValidator formValidator;
    private PermissionFormRelaxedValidator relaxedFormValidator;
    private PermissionDtoValidator dtoValidator;
    private PermissionRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(PermissionEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(PermissionDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(PermissionForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(PermissionEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(PermissionFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchPermissionValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(PermissionDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(PermissionForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(PermissionRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(PermissionFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<PermissionVo> entity2DetailedVoList(List<PermissionEntity> permissionEntityList) {
        List<PermissionVo> permissionDetailsList = new ArrayList<>(permissionEntityList.size());
        for(PermissionEntity entity : permissionEntityList) {
            PermissionVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            permissionDetailsList.add(vo);
        }
        return permissionDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<PermissionVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all PermissionEntity by their natural ordering");
        List<PermissionEntity> permissionEntityList = repository.findAll();
        Set<PermissionVo> naturallyOrderedSet = new TreeSet<PermissionVo>(CMP_BY_ID);
        for(PermissionEntity entity : permissionEntityList) {
            PermissionVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} PermissionVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public PermissionVo retrieveDetailsById(long id) throws PermissionException {
        log.info("Requesting PermissionEntity by id: {}", id);
        Optional<PermissionEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No PermissionEntity found by id: {}", id);
            throw new PermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        PermissionEntity entity = optEntity.get();
        PermissionVo vo = entity2VoConverter.convert(entity);
        log.info("Found PermissionVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<PermissionVo> retrieveAllMatchingDetailsByResource(Long resourceId) throws PermissionException {
        log.info("Requesting PermissionEntity that match with resourceId: {}", resourceId);
        List<PermissionEntity> permissionEntityList = repository.findByResourceId(resourceId);
        if(permissionEntityList != null && !permissionEntityList.isEmpty()) {
            List<PermissionVo> matchedPermissionList = entity2DetailedVoList(permissionEntityList);
            log.info("Found {} PermissionVo matching with resourceId: {}", matchedPermissionList.size(),resourceId);
            return matchedPermissionList;
        }
        log.debug("No PermissionVo found matching with resourceId: {}", resourceId);
        throw new PermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "resourceId", resourceId });
    }

    @Override
    @Transactional(readOnly = true)
    public List<PermissionVo> retrieveAllMatchingDetailsByOperation(Long operationId) throws PermissionException {
        log.info("Requesting PermissionEntity that match with operationId: {}", operationId);
        List<PermissionEntity> permissionEntityList = repository.findByOperationId(operationId);
        if(permissionEntityList != null && !permissionEntityList.isEmpty()) {
            List<PermissionVo> matchedPermissionList = entity2DetailedVoList(permissionEntityList);
            log.info("Found {} PermissionVo matching with operationId: {}", matchedPermissionList.size(),operationId);
            return matchedPermissionList;
        }
        log.debug("No PermissionVo found matching with operationId: {}", operationId);
        throw new PermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "operationId", operationId });
    }

    @Override
    @Transactional
    public Long createPermission(PermissionForm form) throws PermissionException {
        log.info("Creating new PermissionEntity");

        if(form == null) {
            log.debug("PermissionForm provided is null");
            throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of PermissionForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("PermissionForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PermissionForm error detail: {}", ec);
            throw new PermissionException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of PermissionForm are valid");

        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                form.getResourceId(), form.getOperationId());
        if(repository.existsByResourceIdAndOperationId(form.getResourceId(), form.getOperationId())) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    form.getResourceId(), form.getOperationId());
            throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "resourceId", form.getResourceId(), "operationId", form.getOperationId() });
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                form.getResourceId(), form.getOperationId());

        PermissionEntity expectedEntity = form2EntityConverter.convert(form);
        log.debug("Saving {}", expectedEntity);
        PermissionEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new PermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist PermissionForm details" });
        }
        log.info("Created new PermissionForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updatePermission(Long id, PermissionForm form) throws PermissionException {
        log.info("Updating PermissionForm by id: {}", id);

        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PERMISSION_ENTITY_ID.getValue(), id);
        Optional<PermissionEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_NO_PERMISSION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_FOUND_PERMISSION_ENTITY_ID.getValue(), id);

        PermissionEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("PermissionEntity is inactive with id: {}", id);
            throw new PermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PermissionEntity is active with id: {}", id);

        if(form == null) {
            log.debug("PermissionForm is null");
            throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of PermissionForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("PermissionForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PermissionForm error detail: {}", ec);
            throw new PermissionException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of PermissionForm are empty");
            throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of PermissionForm are valid");

        PermissionEntity expectedEntity = null;

        try {
            Optional<PermissionEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
            if(optExpectedEntity.isEmpty()) {
                log.debug("No new value for attributes of PermissionForm");
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
            }
            log.debug("Successfully compared and copied attributes from PermissionForm to PermissionEntity");
            expectedEntity = optExpectedEntity.get();
        } catch (TOABBaseException e) {
            throw (PermissionException) e;
        }

        checkUniquenessOfPermissionModel(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from PermissionEntity to PermissionForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new PermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist  permission details" });
        }
        log.info("Updated existing PermissionEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deletePermission(Long id) throws PermissionException {
        log.info("Soft deleting PermissionEntity by id: {}", id);

        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PERMISSION_ENTITY_ID.getValue(), id);
        Optional<PermissionEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_NO_PERMISSION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_FOUND_PERMISSION_ENTITY_ID.getValue(), id);

        PermissionEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("PermissionEntity is inactive with id: {}", id);
            throw new PermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PermissionEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        PermissionEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new PermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current permission details with id:" + id });
        }

        log.info("Soft deleted existing PermissionEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnPermission(Long id, List<PatchOperationForm> patches) throws PermissionException {
        log.info("Patching PermissionEntity by id: {}", id);

        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PERMISSION_ENTITY_ID.getValue(), id);
        Optional<PermissionEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_NO_PERMISSION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_FOUND_PERMISSION_ENTITY_ID.getValue(), id);

        PermissionEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Permission patch list not provided");
            throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Permission patch list has {} items", patches.size());


        log.debug("Validating patch list items for Permission");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All Permission patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Permission patch item are invalid");
            throw new PermissionException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Permission");


        log.debug("Patching list items to PermissionDto");
        PermissionDto patchedPermissionForm = new PermissionDto();
        try {
            log.debug("Preparing patch list items for Permission");
            JsonNode permissionDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch permissionPatch = JsonPatch.fromJson(permissionDtoTree);
            log.debug("Prepared patch list items for Permission");
            JsonNode blankPermissionDtoTree = om.convertValue(new PermissionDto(), JsonNode.class);
            JsonNode patchedPermissionFormTree = permissionPatch.apply(blankPermissionDtoTree);
            log.debug("Applying patch list items to PermissionDto");
            patchedPermissionForm = om.treeToValue(patchedPermissionFormTree, PermissionDto.class);
            log.debug("Applied patch list items to PermissionDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to PermissionDto: {}", e);
            PermissionException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in PermissionDto");
                ex = new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new PermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to PermissionDto: {}", e);
            throw new PermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to PermissionDto");

        log.debug("Validating patched PermissionDto");
        Errors err = new DirectFieldBindingResult(patchedPermissionForm, patchedPermissionForm.getClass().getSimpleName());
        dtoValidator.validate(patchedPermissionForm, err);
        if(err.hasErrors()) {
            log.debug("Patched PermissionDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched PermissionDto error detail: {}", ec);
            throw new PermissionException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched PermissionDto are valid");

        checkUniquenessOfPermissionModel(patchedPermissionForm, actualEntity);

        log.debug("Comparatively copying patched attributes from PermissionDto to PermissionEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedPermissionForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (PermissionException) e;
        }
        log.debug("Comparatively copied patched attributes from PermissionDto to PermissionEntity");

        log.debug("Saving patched PermissionEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched PermissionEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete PermissionEntity with id:{}", id);
            throw new PermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch  permission details with id:" + id });
        }
        log.info("Patched PermissionEntity with id:{}", id);
    }

    private void checkUniquenessOfPermissionModel(PermissionDto patchedPermissionForm, PermissionEntity actualEntity) throws PermissionException {
        // resourceId = true, operationId = false
        if(patchedPermissionForm.getResourceId().isPresent() && patchedPermissionForm.getOperationId().isEmpty()) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    patchedPermissionForm.getResourceId().get(), actualEntity.getOperation().getId());
            Long resourceId = Long.parseLong(patchedPermissionForm.getResourceId().get());
            boolean duplicateEntitySw =  repository.existsByResourceIdAndOperationId(resourceId, actualEntity.getOperation().getId());
            if(duplicateEntitySw) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                        patchedPermissionForm.getResourceId().get(), actualEntity.getOperation().getId());
                throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "resourceId " + patchedPermissionForm.getResourceId().get(), "operationId " + actualEntity.getOperation().getId() });
            }
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    patchedPermissionForm.getResourceId().get(), actualEntity.getOperation().getId());
        }

        // resourceId = false, operationId = true
        if(patchedPermissionForm.getResourceId().isEmpty() && patchedPermissionForm.getOperationId().isPresent()) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    actualEntity.getResource().getId(), patchedPermissionForm.getOperationId().get());
            Long operationId = Long.parseLong(patchedPermissionForm.getOperationId().get());
            boolean duplicateEntitySw =  repository.existsByResourceIdAndOperationId(actualEntity.getResource().getId(), operationId);
            if(duplicateEntitySw) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                        actualEntity.getResource().getId(), patchedPermissionForm.getOperationId().get());
                throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "resourceId " + actualEntity.getResource().getId(), "operationId " + patchedPermissionForm.getOperationId().get() });
            }
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    actualEntity.getResource().getId(), patchedPermissionForm.getOperationId().get());
        }

        // resourceId = true, operationId = true
        if(patchedPermissionForm.getResourceId().isEmpty() && patchedPermissionForm.getOperationId().isPresent()) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    patchedPermissionForm.getResourceId().get(), patchedPermissionForm.getOperationId().get());
            Long resourceId = Long.parseLong(patchedPermissionForm.getResourceId().get());
            Long operationId = Long.parseLong(patchedPermissionForm.getOperationId().get());
            boolean duplicateEntitySw =  repository.existsByResourceIdAndOperationId(resourceId, operationId);
            if(duplicateEntitySw) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                        patchedPermissionForm.getResourceId().get(), patchedPermissionForm.getOperationId().get());
                throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "resourceId " + patchedPermissionForm.getResourceId().get(), "operationId " + patchedPermissionForm.getOperationId().get() });
            }
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    patchedPermissionForm.getResourceId().get(), patchedPermissionForm.getOperationId().get());
        }
    }

    private void checkUniquenessOfPermissionModel(PermissionForm permissionForm, PermissionEntity actualEntity) throws PermissionException {
        // resourceId = true, operationId = false
        if(permissionForm.getResourceId() != null && permissionForm.getOperationId() == null) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    permissionForm.getResourceId(), actualEntity.getOperation().getId());
            boolean duplicateEntitySw =  repository.existsByResourceIdAndOperationId(permissionForm.getResourceId(), actualEntity.getOperation().getId());
            if(duplicateEntitySw) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                        permissionForm.getResourceId(), actualEntity.getOperation().getId());
                throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "resourceId " + permissionForm.getResourceId(), "operationId " + actualEntity.getOperation().getId() });
            }
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    permissionForm.getResourceId(), actualEntity.getOperation().getId());
        }

        // resourceId = false, operationId = true
        if(permissionForm.getResourceId() == null && permissionForm.getOperationId() != null) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    actualEntity.getResource().getId(), permissionForm.getOperationId());
            boolean duplicateEntitySw =  repository.existsByResourceIdAndOperationId(actualEntity.getResource().getId(), permissionForm.getOperationId());
            if(duplicateEntitySw) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                        actualEntity.getResource().getId(), permissionForm.getOperationId());
                throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "resourceId " + actualEntity.getResource().getId(), "operationId " + permissionForm.getOperationId() });
            }
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    actualEntity.getResource().getId(), permissionForm.getOperationId());
        }

        // resourceId = true, operationId = true
        if(permissionForm.getResourceId() != null && permissionForm.getOperationId() != null) {
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    permissionForm.getResourceId(), permissionForm.getOperationId());
            boolean duplicateEntitySw =  repository.existsByResourceIdAndOperationId(permissionForm.getResourceId(), permissionForm.getOperationId());
            if(duplicateEntitySw) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                        permissionForm.getResourceId(), permissionForm.getOperationId());
                throw new PermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "resourceId " + permissionForm.getResourceId(), "operationId " + permissionForm.getOperationId() });
            }
            log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID.getValue(),
                    permissionForm.getResourceId(), permissionForm.getOperationId());
        }
    }

}