package com.teenthofabud.laundromat.manager.type.model.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelMessageTemplate;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.model.service.TypeModelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "Type Model API", description = "Manage Type Models and their details")
public class TypeModelController {

    @Autowired
    public void setService(TypeModelService service) {
        this.service = service;
    }

    private TypeModelService service;

    @Operation(summary = "Create new Type Model details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type Model attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Type Model already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Type Model attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTypeModel(@RequestBody(required = false) TypeModelForm form) throws TypeModelException {
        log.debug("Requesting to create new type model");
        if(form != null) {
            Long id = service.createTypeModel(form);
            log.debug("Responding with identifier of newly created new type model");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TypeModelForm is null");
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Type Model details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type Model attribute's value is invalid/Type Model is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Type Model already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Type Model details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTypeModel(@PathVariable String id, @RequestBody(required = false) TypeModelForm form)
            throws TypeModelException {
        log.debug("Requesting to update all attributes of existing type model");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_VALID, id);
                if(form != null) {
                    service.updateTypeModel(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing type model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TypeModelForm is null");
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_INVALID, id);
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_EMPTY);
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Type Model by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type Model id is invalid/Type Model is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Type Model attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Type LOV",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTypeModel(@PathVariable String id) throws TypeModelException {
        log.debug("Requesting to soft delete type model");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_VALID, id);
                service.deleteTypeModel(actualId);
                log.debug("Responding with successful deletion of existing type model");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_INVALID, id);
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_EMPTY);
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Type Model attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Type Model with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Type Model attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Type Model attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Type Model with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTypeModel(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TOABBaseException {
        log.debug("Requesting to patch of type model attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_VALID, id);
                if(dtoList != null) {
                    service.applyPatchOnTypeModel(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing type model");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("type model patch document is null");
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_INVALID, id);
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_EMPTY);
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Type Model details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Type Models and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = TypeModelVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<TypeModelVo> getAllTypeModelNaturallyOrdered() {
        log.debug("Requesting all available type models by their natural orders");
        Set<TypeModelVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available type models by their natural orders");
        return naturallyOrderedStudents;
    }

    @Operation(summary = "Get all Type Model details by name")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Type Models and their details that match the given name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
                            array = @ArraySchema(schema = @Schema(implementation = TypeModelVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Type Model name is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type Models available with the given name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<TypeModelVo> getAllStudentsByName(@PathVariable String name) throws TypeModelException {
        log.debug("Requesting all available type models with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<TypeModelVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available type models with given name");
            return matchedByNames;
        }
        log.debug("type model name is empty");
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Type Model details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Type Model that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TypeModelVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Type Model id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type Model found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public TypeModelVo getTypeModelDetailsById(@PathVariable String id) throws TypeModelException {
        log.debug("Requesting all available type models by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_VALID, id);
                TypeModelVo studentDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing type model details by id");
                return studentDetails;
            } catch (NumberFormatException e) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_INVALID, id);
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_ID_EMPTY);
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Type Model details by typeLovId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Type Model that belong to the given typeLovId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
                            array = @ArraySchema(schema = @Schema(implementation = TypeModelVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Type Model typeLovId is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Type Model belongs to the given typeLovId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("typelovid/{typeLovId}")
    public List<TypeModelVo> getTypeModelDetailsByTypeLOVId(@PathVariable String typeLovId) throws TypeModelException {
        log.debug("Requesting all available type models by typeLovId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(typeLovId))) {
            try {
                Long actualTypeLovId = Long.parseLong(typeLovId);
                log.debug("typeLovId: {} is semantically valid", typeLovId);
                List<TypeModelVo> typeModelDetails = service.retrieveDetailsByTypeLOVId(actualTypeLovId);
                log.debug("Responding with successful retrieval of all available type model details by typeLovId");
                return typeModelDetails;
            } catch (NumberFormatException e) {
                log.debug("typeLovId: {} is invalid", typeLovId);
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId", typeLovId });
            }
        }
        log.debug("typeLovId is empty");
        throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId", typeLovId });
    }

}
