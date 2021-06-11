package com.teenthofabud.laundromat.manager.type.model.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.type.constant.TypeSubDomain;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.model.service.TypeModelService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("model")
@Slf4j
public class TypeModelController {

    @Autowired
    public void setService(TypeModelService service) {
        this.service = service;
    }

    private TypeModelService service;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTypeModel(@RequestBody(required = false) TypeModelForm form) throws TypeException {
        log.debug("Requesting to create new type model");
        if(form != null) {
            Long id = service.createTypeModel(form);
            log.debug("Responding with identifier of newly created new type model");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TypeModelForm is null");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTypeModel(@PathVariable String id, @RequestBody(required = false) TypeModelForm form)
            throws TypeException {
        log.debug("Requesting to update all attributes of existing type model");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type model id: {} is semantically valid", id);
                if(form != null) {
                    service.updateTypeModel(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing type model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TypeModelForm is null");
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug("type model id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type model id is empty");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTypeModel(@PathVariable String id) throws TypeException {
        log.debug("Requesting to soft delete type model");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type model id: {} is semantically valid", id);
                service.deleteTypeModel(actualId);
                log.debug("Responding with successful deletion of existing type model");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug("type model id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type model id is empty");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTypeModel(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TOABBaseException {
        log.debug("Requesting to patch of type model attributes");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type model id: {} is semantically valid", id);
                if(dtoList != null) {
                    service.applyPatchOnTypeModel(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing type model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("type model patch document is null");
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug("type model id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type model id is empty");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping
    public Set<TypeModelVo> getAllTypeModelNaturallyOrdered() {
        log.debug("Requesting all available type models by their natural orders");
        Set<TypeModelVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available type models by their natural orders");
        return naturallyOrderedStudents;
    }

    @GetMapping("name/{name}")
    public List<TypeModelVo> getAllStudentsByName(@PathVariable String name) throws TypeException {
        log.debug("Requesting all available type models with given name");
        if(StringUtils.hasText(name)) {
            List<TypeModelVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available type models with given name");
            return matchedByNames;
        }
        log.debug("type model name is empty");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @GetMapping("{id}")
    public TypeModelVo getTypeModelDetailsById(@PathVariable String id) throws TypeException {
        log.debug("Requesting all available type models by its id");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type model id: {} is semantically valid", id);
                TypeModelVo studentDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing type model details by id");
                return studentDetails;
            } catch (NumberFormatException e) {
                log.debug("type model id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type model id is empty");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping("typelovid/{typeLovId}")
    public List<TypeModelVo> getTypeModelDetailsByTypeLOVId(@PathVariable String typeLovId) throws TypeException {
        log.debug("Requesting all available type models by typeLovId");
        if(StringUtils.hasText(typeLovId)) {
            try {
                Long actualTypeLovId = Long.parseLong(typeLovId);
                log.debug("typeLovId: {} is semantically valid", typeLovId);
                List<TypeModelVo> typeModelDetails = service.retrieveDetailsByTypeLOVId(actualTypeLovId);
                log.debug("Responding with successful retrieval of all available type model details by typeLovId");
                return typeModelDetails;
            } catch (NumberFormatException e) {
                log.debug("typeLovId: {} is invalid", typeLovId);
                throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId", typeLovId });
            }
        }
        log.debug("typeLovId is empty");
        throw new TypeException(TypeSubDomain.MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId", typeLovId });
    }

}
