package com.teenthofabud.laundromat.manager.type.model.controller;

import com.teenthofabud.core.common.model.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.core.common.model.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.constant.TypeSubDomain;
import com.teenthofabud.laundromat.manager.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.error.TypeException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.model.service.TypeModelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("typemodel")
public class TypeModelManagementController {

    @Autowired
    public void setService(TypeModelService service) {
        this.service = service;
    }

    private TypeModelService service;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTypeModel(@RequestBody(required = false) TypeModelForm form) throws TypeException {
        if(form != null) {
            long id = service.createTypeModel(form);
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTypeModel(@PathVariable String id,
                                                           @RequestBody(required = false) TypeModelForm form) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                if(form != null) {
                    service.updateTypeModel(actualId, form);
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTypeModel(@PathVariable String id) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                service.deleteTypeModel(actualId);
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTypeModel(@PathVariable String id,
                                                             @RequestBody(required = false) List<PatchOperationForm> dtoList) throws TOABBaseException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                if(dtoList != null) {
                    service.applyPatchOnTypeModel(actualId, dtoList);
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping
    public Set<TypeModelVo> getAllTypeModelNaturallyOrdered() {
        Set<TypeModelVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        return naturallyOrderedStudents;
    }

    @GetMapping("name/{name}")
    public List<TypeModelVo> getAllStudentsByName(@PathVariable String name) throws TypeException {
        if(StringUtils.hasText(name)) {
            List<TypeModelVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            return matchedByNames;
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @GetMapping("id/{id}")
    public TypeModelVo getTypeModelDetailsById(@PathVariable String id) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                TypeModelVo studentDetails = service.retrieveDetailsById(actualId);
                return studentDetails;
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping("typelovid/{typelovid}")
    public List<TypeModelVo> getTypeModelDetailsByTypeLOVId(@PathVariable String typelovid) throws TypeException {
        if(StringUtils.hasText(typelovid)) {
            try {
                long actualTypeLovId = Long.parseLong(typelovid);
                List<TypeModelVo> typeModelDetails = service.retrieveDetailsByTypeLOVId(actualTypeLovId);
                return typeModelDetails;
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_MODEL, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typelovid", typelovid });
    }

}