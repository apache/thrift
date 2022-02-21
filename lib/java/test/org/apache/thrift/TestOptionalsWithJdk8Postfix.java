package org.apache.thrift;

import junit.framework.TestCase;
import thrift.test.optiontype.jdk8withpostfix.Person;

public class TestOptionalsWithJdk8Postfix extends TestCase {

  public void testConstruction() {
    Person person = new Person(1L, "name");
    assertFalse(person.getAgeOptional().isPresent());
    assertEquals(0L, person.getAge());
    assertFalse(person.isSetAge());
    assertFalse(person.getPhoneOptional().isPresent());
    assertFalse(person.isSetPhone());
    assertEquals(1L, person.getId());
    assertTrue(person.isSetId());
    assertEquals("name", person.getName());
    assertTrue(person.isSetName());

    assertFalse(person.getAddressesOptional().isPresent());
    assertEquals(Integer.valueOf(0), person.getAddressesSizeOptional().orElse(0));
    assertFalse(person.getPetsOptional().isPresent());
    assertEquals(Integer.valueOf(0), person.getPetsSizeOptional().orElse(0));
  }

  public void testEmpty() {
    Person person = new Person();
    person.setPhone("phone");
    assertFalse(person.getAgeOptional().isPresent());
    assertFalse(person.isSetAge());
    assertTrue(person.getPhoneOptional().isPresent());
    assertEquals("phone", person.getPhoneOptional().get());
    assertTrue(person.isSetPhone());
    assertEquals(0L, person.getId());
    assertFalse(person.isSetId());
    assertNull(person.getName());
    assertFalse(person.isSetName());

    assertFalse(person.getAddressesOptional().isPresent());
    assertEquals(Integer.valueOf(0), person.getAddressesSizeOptional().orElse(0));
    assertFalse(person.getPetsOptional().isPresent());
    assertEquals(Integer.valueOf(0), person.getPetsSizeOptional().orElse(0));
  }
}
