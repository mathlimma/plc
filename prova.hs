--q1
{--
import Control.Concurrent
geraLampada :: (Eq a, Eq a1, Num a, Num a1, Num a2, Show a2) =>
 MVar a -> MVar a1 -> MVar a2 -> t -> IO b
geraLampada bulbo socket lampadas maxLampadas = do

 b <- takeMVar bulbo
 s <- takeMVar socket
 if (b == 0 || s == 0) then do
 putMVar bulbo b
 putMVar socket s
 geraLampada bulbo socket lampadas maxLampadas --aguarda
 else do
 putMVar bulbo (b-1)
 putMVar socket (s-1)
 l <- takeMVar lampadas
 putStrLn("Gerando lampada " ++ (show $ l+1))
 putMVar lampadas (l+1)
 geraLampada bulbo socket lampadas maxLampadas
transporte :: (Num a, Ord a) => MVar a -> t -> IO b
transporte lampadas fim = do
 l <- takeMVar lampadas
 if l >= 50 then do
 putStrLn("Transportando lampadas")
 putMVar lampadas 0
 transporte lampadas fim
 else do
 putMVar lampadas l
 transporte lampadas fim
geraBulboLampada :: (Eq a, Num a) => MVar a -> a -> t -> IO b
geraBulboLampada bulORLam limite fim = do
 b <- takeMVar bulORLam
 if (b == limite) then do
 putMVar bulORLam b
 geraBulboLampada bulORLam limite fim --aguarda
 else do
 putMVar bulORLam (b+1)
 geraBulboLampada bulORLam limite fim
main :: IO()
main = do
 socket <- newMVar 0
 bulbo <- newMVar 0
 lampadas <- newMVar 0
 forkIO (geraBulboLampada bulbo 50 0)
 forkIO (geraBulboLampada socket 50 0)
 forkIO (geraLampada bulbo socket lampadas 0)
 transporte lampadas 0


//q2
//Aux.java
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
public class Aux{
 int valor;
 boolean cheio;
 Lock lock = new ReentrantLock();
 public Aux(){
 valor = 0;
 cheio = false;
 }
 public synchronized void put(int i){
 while ( cheio ){
 try{
 wait();
 } catch (Exception e){}
 }
 cheio = true;
 valor = i;
 System.out.println("Colocou " + valor);
 notifyAll();
 }
 public synchronized int take(){
 while (!cheio){
 try{
 wait();
 } catch (Exception e){}
 }
 cheio = false;
 System.out.println("Pegou " + valor);
 notifyAll();
 return valor;
 }
}
//Consumidor.java
public class Consumidor implements Runnable{
 int id;
 Aux a;
 public Consumidor(int id, Aux a ){
 this.id = id;
 this.a = a;
 }
 public void run(){
 for (int i=0; i<10; i++){
 int n = a.take();
 // System.out.println("Consumidor " + id + " pega " + n);
 }
 System.out.println("Consumidor termina");
 }
}
--Produtor.java
public class Produtor implements Runnable{
 int id;
 Aux a;
 public Produtor(int id, Aux a){
 this.a = a;
 }
 public void run(){
 for (int i=0; i< 10; i++){
 a.put(i);
 --System.out.pri

 Thread p = new Thread(new Produtor(1, a));
 Thread c = new Thread( new Consumidor(1, a));
 p.start();
 c.start();
 try{
 p.join();
 c.join();
 } catch (Exception e){}
 }
}
//q3
//Aux.java
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
// import java.util.concurrent.locks.Lock;
// import java.util.concurrent.locks.ReentrantLock;
public class Aux{
 AtomicInteger valor;
 AtomicBoolean cheio;
 // Lock lock = new ReentrantLock();
 public Aux(){
 valor = new AtomicInteger(0);
 cheio = new AtomicBoolean(false);
 }
 public synchronized void put(int i){
 while ( cheio.get() ){
 try{
 wait();
 } catch (Exception e){}
 }
 cheio.set(true);
 valor.set(i);
 System.out.println("Colocou " + valor);
 notifyAll();
 }
 public synchronized int take(){
 while (!cheio.get()){
 try{
 wait();
 } catch (Exception e){}
 }
 cheio.set(false);
 System.out.println("Pegou " + valor);
 notifyAll();
 return valor.get();
 }
}--}
--q4
import Control.Concurrent.STM
type Conta = TVar Int

saque :: Conta -> Int -> STM()
saque conta valor = do
 t <- readTVar conta
 writeTVar conta (t-valor)

deposito :: Conta -> Int -> STM()
deposito conta valor = do
 saque conta (-valor)

saque2 :: Conta -> Int -> STM()
saque2 conta valor = do
 t <- readTVar conta
 if (t - valor < 0) then
  saque2 conta valor
 else
  writeTVar conta (t-valor)