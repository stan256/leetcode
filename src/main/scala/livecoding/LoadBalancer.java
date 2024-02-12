package livecoding;

import java.util.*;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;


class Runner {
    public static void main(String[] args) {
        List<String> list = Arrays.asList("1", "5", "19");
        LoadBalancer loadBalancer = new NonThreadSafeRoundRobinLoadBalancing(list);

        for (int i = 0; i < Byte.MAX_VALUE + 1; i++) {
            System.out.println(loadBalancer.getIp());
        }
    }
}

public abstract class LoadBalancer {
    final List<String> ipList;

    public LoadBalancer(List<String> ipList) {
        this.ipList = Collections.unmodifiableList(ipList);
    }

    abstract String getIp();
}

class RoundRobinLoadBalancer extends LoadBalancer {
    private ReentrantLock lock = new ReentrantLock();
    private int counter = 0;

    public RoundRobinLoadBalancer(List<String> ipList) {
        super(ipList);
    }

    @Override
    String getIp() {
        lock.lock();
        try {
            return this.ipList.get(Math.abs(counter++ % this.ipList.size()));
        } finally {
            lock.unlock();
        }
    }
}

class WeightedRoundRobinLoadBalancer extends RoundRobinLoadBalancer {

    public WeightedRoundRobinLoadBalancer(Map<String, Integer> ipMap) {
        super(
                ipMap.keySet()
                        .stream()
                        .map(ip -> {
                            List<String> tempList = new LinkedList<>();
                            for (int i = 0; i < ipMap.get(ip); i++) {
                                tempList.add(ip);
                            }
                            return tempList;
                        })
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList())
        );
    }
}

class NonThreadSafeRoundRobinLoadBalancing extends LoadBalancer {
    private byte counter = 0;

    public NonThreadSafeRoundRobinLoadBalancing(List<String> servers) {
        super(servers);
    }

    @Override
    String getIp() {
        return this.ipList.get(Math.abs(counter++ % ipList.size()));
    }
}
