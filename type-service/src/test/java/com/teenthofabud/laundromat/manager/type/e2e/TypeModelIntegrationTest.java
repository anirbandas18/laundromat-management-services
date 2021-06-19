package com.teenthofabud.laundromat.manager.type.e2e;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class TypeModelIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String TYPE_MODEL_URI = "/model";
    private static final String TYPE_MODEL_URI_BY_ID = "/model/{id}";
    private static final String TYPE_MODEL_URI_BY_NAME = "/model/name/{name}";
    private static final String TYPE_MODEL_URI_BY_TYPE_LOV_ID = "/model/typelovid/{typeLovId}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private TypeLOVVo typeLOVVo1;
    private TypeLOVVo typeLOVVo2;
    private TypeLOVVo typeLOVVo3;
    private TypeLOVEntity typeLOVEntity1;
    private TypeLOVEntity typeLOVEntity2;
    private TypeLOVEntity typeLOVEntity3;

    private TypeModelForm typeModelForm1;
    private TypeModelForm typeModelForm2;
    private TypeModelVo typeModelVo1;
    private TypeModelVo typeModelVo2;
    private TypeModelVo typeModelVo3;
    private TypeModelVo typeModelVo4;
    private TypeModelVo typeModelVo5;
    private TypeModelVo typeModelVo6;
    private TypeModelEntity typeModelEntity1;
    private TypeModelEntity typeModelEntity2;
    private TypeModelEntity typeModelEntity3;
    private TypeModelEntity typeModelEntity4;
    private TypeModelEntity typeModelEntity5;
    private TypeModelEntity typeModelEntity6;
    private List<PatchOperationForm> patches;

    private void setUpLOV() {
        typeLOVVo1 = new TypeLOVVo();
        typeLOVVo1.setId(1L);
        typeLOVVo1.setActive(Boolean.TRUE);
        typeLOVVo1.setName("Test Type LOV 1");
        typeLOVVo1.setDescription("This belongs to group 1 for e2e testing");

        typeLOVVo2 = new TypeLOVVo();
        typeLOVVo2.setId(2L);
        typeLOVVo2.setActive(Boolean.TRUE);
        typeLOVVo2.setName("Test Type LOV 2");
        typeLOVVo2.setDescription("This belongs to group 2 for e2e testing");

        typeLOVVo3 = new TypeLOVVo();
        typeLOVVo3.setId(3L);
        typeLOVVo3.setActive(Boolean.TRUE);
        typeLOVVo3.setName("Test Type LOV 3");
        typeLOVVo3.setDescription("This belongs to group 3 for e2e testing");

        typeLOVEntity1 = new TypeLOVEntity();
        typeLOVEntity1.setActive(Boolean.TRUE);
        typeLOVEntity1.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity1.setCreatedOn(LocalDateTime.now());
        typeLOVEntity1.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity1.setModifiedOn(LocalDateTime.now());
        typeLOVEntity1.setVersion(0);
        typeLOVEntity1.setName("Test Type LOV 1");
        typeLOVEntity1.setDescription("This belongs to group 1 for e2e testing");

        typeLOVEntity2 = new TypeLOVEntity();
        typeLOVEntity2.setActive(Boolean.TRUE);
        typeLOVEntity2.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity2.setCreatedOn(LocalDateTime.now());
        typeLOVEntity2.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity2.setModifiedOn(LocalDateTime.now());
        typeLOVEntity2.setVersion(0);
        typeLOVEntity2.setName("Test Type LOV 2");
        typeLOVEntity2.setDescription("This belongs to group 2 for e2e testing");

        typeLOVEntity3 = new TypeLOVEntity();
        typeLOVEntity3.setActive(Boolean.FALSE);
        typeLOVEntity3.setCreatedBy(CREATED_BY_USER_ID);
        typeLOVEntity3.setCreatedOn(LocalDateTime.now());
        typeLOVEntity3.setModifiedBy(CREATED_BY_USER_ID);
        typeLOVEntity3.setModifiedOn(LocalDateTime.now());
        typeLOVEntity3.setVersion(0);
        typeLOVEntity3.setName("Test Type LOV 3");
        typeLOVEntity3.setDescription("This belongs to group 3 for e2e testing");
    }

    @BeforeAll
    private void setUp() {
        setUpLOV();

        typeModelForm1 = new TypeModelForm();
        typeModelForm1.setName("Demo Model");
        typeModelForm1.setDescription("This is for e2e testing of services");
        typeModelForm1.setTypeLovId(1L);

        typeModelForm2 = new TypeModelForm();
        typeModelForm2.setName("Another Model");
        typeModelForm2.setDescription("This is for e2e testing of services");
        typeModelForm2.setTypeLovId(2L);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Model"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Type Model resource"));

        typeModelVo1 = new TypeModelVo();
        typeModelVo1.setId(1L);
        typeModelVo1.setActive(Boolean.TRUE);
        typeModelVo1.setName("Test Type Model 1");
        typeModelVo1.setDescription("This belongs to group 1 for e2e testing");
        typeModelVo1.setTypeLov(typeLOVVo1);

        typeModelVo2 = new TypeModelVo();
        typeModelVo2.setId(2L);
        typeModelVo2.setActive(Boolean.TRUE);
        typeModelVo2.setName("Test Type Model 2");
        typeModelVo2.setDescription("This belongs to group 1 for e2e testing");
        typeModelVo2.setTypeLov(typeLOVVo1);

        typeModelVo3 = new TypeModelVo();
        typeModelVo3.setId(3L);
        typeModelVo3.setActive(Boolean.TRUE);
        typeModelVo3.setName("Sample Type Model 3");
        typeModelVo3.setDescription("This belongs to group 2 for e2e testing");
        typeModelVo3.setTypeLov(typeLOVVo2);

        typeModelVo4 = new TypeModelVo();
        typeModelVo4.setId(4L);
        typeModelVo4.setActive(Boolean.FALSE);
        typeModelVo4.setName("Sample Type Model 4");
        typeModelVo4.setDescription("This belongs to group 2 for e2e testing");
        typeModelVo4.setTypeLov(typeLOVVo2);

        typeModelVo5 = new TypeModelVo();
        typeModelVo5.setId(5L);
        typeModelVo5.setActive(Boolean.FALSE);
        typeModelVo5.setName("Sample Type Model 5");
        typeModelVo5.setDescription("This belongs to group 3 for e2e testing");
        typeModelVo5.setTypeLov(typeLOVVo3);

        typeModelVo6 = new TypeModelVo();
        typeModelVo6.setId(6L);
        typeModelVo6.setActive(Boolean.FALSE);
        typeModelVo6.setName("Sample Type Model 6");
        typeModelVo6.setDescription("This belongs to group 3 for e2e testing");
        typeModelVo6.setTypeLov(typeLOVVo3);

        typeModelEntity1 = new TypeModelEntity();
        typeModelEntity1.setActive(Boolean.TRUE);
        typeModelEntity1.setCreatedBy(CREATED_BY_USER_ID);
        typeModelEntity1.setCreatedOn(LocalDateTime.now());
        typeModelEntity1.setModifiedBy(CREATED_BY_USER_ID);
        typeModelEntity1.setModifiedOn(LocalDateTime.now());
        typeModelEntity1.setVersion(0);
        typeModelEntity1.setName("Test Type Model 1");
        typeModelEntity1.setDescription("This belongs to group 1 for e2e testing");

        typeModelEntity2 = new TypeModelEntity();
        typeModelEntity2.setActive(Boolean.TRUE);
        typeModelEntity2.setCreatedBy(CREATED_BY_USER_ID);
        typeModelEntity2.setCreatedOn(LocalDateTime.now());
        typeModelEntity2.setModifiedBy(CREATED_BY_USER_ID);
        typeModelEntity2.setModifiedOn(LocalDateTime.now());
        typeModelEntity2.setVersion(0);
        typeModelEntity2.setName("Test Type Model 2");
        typeModelEntity2.setDescription("This belongs to group 1 for e2e testing");

        typeModelEntity3 = new TypeModelEntity();
        typeModelEntity3.setActive(Boolean.TRUE);
        typeModelEntity3.setCreatedBy(CREATED_BY_USER_ID);
        typeModelEntity3.setCreatedOn(LocalDateTime.now());
        typeModelEntity3.setModifiedBy(CREATED_BY_USER_ID);
        typeModelEntity3.setModifiedOn(LocalDateTime.now());
        typeModelEntity3.setVersion(0);
        typeModelEntity3.setName("Sample Type Model 3");
        typeModelEntity3.setDescription("This belongs to group 2 for e2e testing");

        typeModelEntity4 = new TypeModelEntity();
        typeModelEntity4.setActive(Boolean.FALSE);
        typeModelEntity4.setCreatedBy(CREATED_BY_USER_ID);
        typeModelEntity4.setCreatedOn(LocalDateTime.now());
        typeModelEntity4.setModifiedBy(CREATED_BY_USER_ID);
        typeModelEntity4.setModifiedOn(LocalDateTime.now());
        typeModelEntity4.setVersion(0);
        typeModelEntity4.setName("Sample Type Model 4");
        typeModelEntity4.setDescription("This belongs to group 2 for e2e testing");

        typeModelEntity5 = new TypeModelEntity();
        typeModelEntity5.setActive(Boolean.FALSE);
        typeModelEntity5.setCreatedBy(CREATED_BY_USER_ID);
        typeModelEntity5.setCreatedOn(LocalDateTime.now());
        typeModelEntity5.setModifiedBy(CREATED_BY_USER_ID);
        typeModelEntity5.setModifiedOn(LocalDateTime.now());
        typeModelEntity5.setVersion(0);
        typeModelEntity5.setName("Sample Type Model 5");
        typeModelEntity5.setDescription("This belongs to group 3 for e2e testing");

        typeModelEntity6 = new TypeModelEntity();
        typeModelEntity6.setActive(Boolean.FALSE);
        typeModelEntity6.setCreatedBy(CREATED_BY_USER_ID);
        typeModelEntity6.setCreatedOn(LocalDateTime.now());
        typeModelEntity6.setModifiedBy(CREATED_BY_USER_ID);
        typeModelEntity6.setModifiedOn(LocalDateTime.now());
        typeModelEntity6.setVersion(0);
        typeModelEntity6.setName("Sample Type Model 6");
        typeModelEntity6.setDescription("This belongs to group 3 for e2e testing");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        typeLOVEntity1 = em.merge(typeLOVEntity1);
        typeLOVEntity2 = em.merge(typeLOVEntity2);
        typeLOVEntity3 = em.merge(typeLOVEntity3);

        typeModelEntity1.setTypeLov(typeLOVEntity1);
        typeModelEntity2.setTypeLov(typeLOVEntity1);
        typeModelEntity3.setTypeLov(typeLOVEntity2);
        typeModelEntity4.setTypeLov(typeLOVEntity2);
        typeModelEntity5.setTypeLov(typeLOVEntity3);
        typeModelEntity6.setTypeLov(typeLOVEntity3);

        em.merge(typeModelEntity1);
        em.merge(typeModelEntity2);
        em.merge(typeModelEntity3);
        em.merge(typeModelEntity4);
        em.merge(typeModelEntity5);
        em.merge(typeModelEntity6);
    }

    @AfterEach
    private void destroy() {
        typeModelEntity1.setTypeLov(null);
        typeModelEntity2.setTypeLov(null);
        typeModelEntity3.setTypeLov(null);
        typeModelEntity4.setTypeLov(null);
        typeModelEntity5.setTypeLov(null);
        typeModelEntity6.setTypeLov(null);

        em.remove(typeModelEntity1);
        em.remove(typeModelEntity2);
        em.remove(typeModelEntity3);
        em.remove(typeModelEntity4);
        em.remove(typeModelEntity5);
        em.remove(typeModelEntity6);

        em.remove(typeLOVEntity1);
        em.remove(typeLOVEntity2);
        em.remove(typeLOVEntity3);

        typeModelForm1 = new TypeModelForm();
        typeModelForm1.setName("Demo Model");
        typeModelForm1.setDescription("This is for e2e testing of services");
        typeModelForm1.setTypeLovId(1L);

        typeModelForm2 = new TypeModelForm();
        typeModelForm2.setName("Another Model");
        typeModelForm2.setDescription("This is for e2e testing of services");
        typeModelForm2.setTypeLovId(2L);

        typeLOVVo3 = new TypeLOVVo();
        typeLOVVo3.setId(3L);
        typeLOVVo3.setActive(Boolean.TRUE);
        typeLOVVo3.setName("Test Type LOV 3");
        typeLOVVo3.setDescription("This belongs to group 3 for e2e testing");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Model"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Type Model resource"));
    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_200Response_And_NewTypeModelId_WhenPosted_WithValidTypeModelForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        typeModelForm1.setName("");

        mvcResult = mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_409Response_And_ErrorCode_LMS_TYPE_004_WhenRequested_WithDuplicateTypeModel() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_EXISTS.getErrorCode();
        String field1Name = "name";
        typeModelForm1.setName("Test Type Model 1");

        mvcResult = mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenPosted_WithNoTypeModelForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenPosted_WithInvalidTypeModelId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String message = "invalid";
        typeModelForm1.setTypeLovId(-1L);

        mvcResult = mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenPosted_WithAbsentTypeModelId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String message = "invalid";
        typeModelForm1.setTypeLovId(11L);

        mvcResult = mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenPosted_WithInactiveTypeModelId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String message = "invalid";
        typeModelForm1.setTypeLovId(3L);

        mvcResult = mockMvc.perform(post(TYPE_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_200Response_And_TypeModelListNaturallyOrdered_WhenRequested_ForAllTypeModels() throws Exception {
        MvcResult mvcResult = null;
        Set<TypeModelVo> studentList = new TreeSet<>(Arrays.asList(typeModelVo1, typeModelVo2, typeModelVo3, typeModelVo4));
        long expectedTypeModelVoCount = 6;

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedTypeModelVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), TypeModelVo[].class).length);
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_200Response_And_TypeModelDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(typeModelVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(typeModelVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TypeModelVo.class).getId());
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ByInvalidId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_200Response_And_MatchingTypeModelDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<TypeModelVo> students = Arrays.asList(typeModelVo1, typeModelVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), TypeModelVo[].class).length);
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_TypeModel_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TYPE_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TypeModel_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TYPE_MODEL_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TYPE_MODEL_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TYPE_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeModel_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TYPE_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTypeModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        typeModelForm1.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_ByEmptyId_AndTypeModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, " ")
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ByInvalidId_AndTypeModelDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenUpdated_ByAbsentId_AndTypeModelDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_005_WhenUpdated_ByInactiveId_AndTypeModelDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_WithInvalidTypeModelId() throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String message = "invalid";
        typeModelForm1.setTypeLovId(-1L);

        mvcResult = mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_WithAbsentTypeModelId() throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String message = "invalid";
        typeModelForm1.setTypeLovId(11L);

        mvcResult = mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_WithInactiveTypeModelId() throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String message = "invalid";
        typeModelForm1.setTypeLovId(3L);

        mvcResult = mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ById_AndNoTypeModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        typeModelForm1.setName("");

        mvcResult = mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ById_AndEmptyTypeModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new TypeModelForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TypeModel_Put_ShouldReturn_409Response_And_ErrorCode_LMS_TYPE_004_WhenUpdated_ById_AndDuplicateTypeModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_EXISTS.getErrorCode();
        String field1Name = "name";
        typeModelForm1.setName(typeModelEntity1.getName());

        mvcResult = mockMvc.perform(put(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(typeModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTypeModelDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndTypeModelDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }


    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_WithInvalidTypeModelId() throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String fieldValue = "-3";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));

        mvcResult = mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_WithAbsentTypeModelId() throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String fieldValue = "33";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));

        mvcResult = mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenUpdated_WithInactiveTypeModelId() throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";
        String fieldValue = "3";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));

        mvcResult = mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ByInvalidId_AndTypeModelDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_TYPE_002_WhenUpdated_ByAbsentId_AndTypeModelDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_TYPE_002_WhenUpdated_ById_AndDuplicateTypeModelDetails() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = typeModelEntity3.getName();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));


        mvcResult = this.mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_TYPE_003_WhenUpdated_ById_AndNoTypeModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", ""));

        mvcResult = mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ById_AndInvalidDefinitionOfTypeModelAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TYPE_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_200Response_And_TypeModelDetails_WhenRequested_ByTypeLOVId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_TYPE_LOV_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(Arrays.asList(typeModelVo1, typeModelVo2)), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), TypeModelVo[].class).length);
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ByEmptyTypeLOVId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_TYPE_LOV_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_001_WhenRequested_ByInvalidTypeLOVId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_TYPE_LOV_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_002_WhenRequested_ByAbsentTypeLOVId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_TYPE_LOV_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TypeModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TYPE_005_WhenRequested_ByInactiveTypeLOVId() throws Exception {
        Long id = 4L;
        MvcResult mvcResult = null;
        String errorCode = TypeErrorCode.TYPE_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "typeLovId";

        mvcResult = this.mockMvc.perform(get(TYPE_MODEL_URI_BY_TYPE_LOV_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

}