package cap.scalasmt

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.{ File, FileInputStream, FileOutputStream
               , InputStream, OutputStream};
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * Persistence of symbolic expressions.
 * @author kuat
 */ 
object Persistence {
  def serialize(e : AnyRef) : Array[Byte] = {
    val baos = new ByteArrayOutputStream(1024);
    val o = new ObjectOutputStream(baos);
    o.writeObject(e);
    baos.toByteArray();
  }

  def deserialize[T] (s : Array[Byte]) : T = {
    val bais = new ByteArrayInputStream(s);
    val input = new ObjectInputStream(bais);
    val obj = input.readObject();
    obj.asInstanceOf[T]
  }

  def readFromFile[T](filename: String): T = {
    val file: File = new File(filename);
    val in: InputStream = new FileInputStream(file);
    val bytes: Array[Byte] = new Array[Byte]((file.length).asInstanceOf[Int]);
    in.read(bytes);
    in.close();
    Persistence.deserialize[T](bytes);
  }
  def writeToFile(obj: AnyRef, filename: String): Unit = {
    val bytes: Array[Byte] = Persistence.serialize(obj);
    val file: File = new File(filename);
    val out: OutputStream = new FileOutputStream(file);
    out.write(bytes);
    out.close();
  }
}

